use ahash::AHashMap;
use crossbeam_queue::ArrayQueue;
use crossbeam_utils::thread::scope;
use crossterm::{
    cursor::{Hide, MoveTo, Show},
    event::{
        poll, read, DisableMouseCapture, EnableMouseCapture, Event, KeyCode, KeyEvent,
        KeyModifiers, MouseEvent, MouseEventKind,
    },
    execute, queue,
    style::{Attribute, SetAttribute},
    terminal::{
        disable_raw_mode, enable_raw_mode, Clear, ClearType, EnterAlternateScreen,
        LeaveAlternateScreen,
    },
    tty::IsTty,
    Result,
};
use std::{
    fs::File,
    io::{ErrorKind, Write},
    path::PathBuf,
    sync::atomic::AtomicBool,
    sync::Arc,
    time::{Duration, Instant},
};
use bumpalo::Bump;

use std::os::unix::prelude::FromRawFd;

struct Args {
    path: Option<PathBuf>,
}

impl Args {
    pub fn parse() -> Option<Self> {
        let mut args = pico_args::Arguments::from_env();

        if args.contains(["-h", "--help"]) {
            println!("rp 0.1.0");
            println!("USAGE: `<command> | rp` or `rp <path>`");
            return None;
        }

        Some(Self {
            path: args.free_from_str().ok(),
        })
    }
}

fn get_input(args: &Args) -> Result<File> {
    if !std::io::stdin().is_tty() {
        unsafe {
            let stdin = File::from_raw_fd(libc::STDIN_FILENO);
            Ok(stdin)
        }
    } else {
        Ok(File::open(args.path.as_deref().expect("No given path"))?)
    }
}

fn get_output() -> File {
    unsafe { File::from_raw_fd(libc::STDOUT_FILENO) }
}

fn push_newline<'b>(b: &'b Bump, tx: &ArrayQueue<&'b [u8]>, line: &[u8]) {
    let line = b.alloc_slice_copy(line);
    
    while tx.push(line).is_err() {
        std::thread::sleep(Duration::from_millis(50));
    }
}

fn read_from_stdin<'b>(
    mut stdin: File,
    b: &'b mut Bump,
    tx: Arc<ArrayQueue<&'b [u8]>>,
) -> Result<()> {
    let mut stdin_buf = [0; 8196];
    let mut start_len = 0;

    loop {
        use std::io::Read;

        let mut buf = match stdin.read(&mut stdin_buf[start_len..]) {
            Ok(l) => &stdin_buf[..start_len + l],
            Err(ref e) if e.kind() == ErrorKind::Interrupted => continue,
            Err(e) => return Err(From::from(e)),
        };

        #[cfg(feature = "logging")]
        log::debug!("Read {} bytes", buf.len() - start_len);

        if start_len == buf.len() {
            #[cfg(feature = "logging")]
            log::info!("EOF");
            if !buf.is_empty() {
                push_newline(b, &tx, buf);
            }
            break Ok(());
        }

        start_len = buf.len();

        loop {
            match memchr::memchr(b'\n', buf) {
                Some(pos) => {
                    let (line, new_buf) = buf.split_at(pos);
                    buf = new_buf.split_first().expect("Must success").1;
                    push_newline(b, &tx, line);
                }
                None => {
                    break;
                }
            }
        }

        let end_len = buf.len();

        if stdin_buf.len() == end_len {
            #[cfg(feature = "logging")]
            log::info!("Too many bytes");
            // send incomplete single line
            push_newline(b, &tx, &buf);
            break Ok(());
        } else if start_len == end_len {
            // no bytes processed
        } else {
            stdin_buf.copy_within((start_len - end_len)..start_len, 0);
            start_len = end_len;
        }
    }
}

#[derive(Clone, Copy, PartialEq, Eq)]
pub enum PromptState {
    Normal,
    Number(usize),
}

impl PromptState {
    pub fn take(&mut self) -> Self {
        std::mem::replace(self, Self::Normal)
    }
}

#[derive(Clone, Copy)]
pub enum KeyBehavior {
    Quit,
    Down,
    Up,
    PageDown,
    PageUp,
    GotoEnd,
    GotoTop,
}

fn default_keymap() -> AHashMap<KeyEvent, KeyBehavior> {
    let mut dict = AHashMap::new();

    macro_rules! keymap {
        ($($modifier:expr => [$(($code:expr, $behavior:expr),)*],)*) => {
            $(
                $(
                    dict.insert(KeyEvent::new($code, $modifier), $behavior);
                )*
            )*
        }
    }

    keymap! {
        KeyModifiers::NONE => [
            (KeyCode::Enter, KeyBehavior::Down),
            (KeyCode::Down, KeyBehavior::Down),
            (KeyCode::Char('j'), KeyBehavior::Down),

            (KeyCode::Up, KeyBehavior::Up),
            (KeyCode::Char('k'), KeyBehavior::Up),

            (KeyCode::PageDown, KeyBehavior::PageDown),

            (KeyCode::Home, KeyBehavior::GotoTop),
            (KeyCode::End, KeyBehavior::GotoEnd),
            (KeyCode::Char('g'), KeyBehavior::GotoTop),

            (KeyCode::Char('q'), KeyBehavior::Quit),
        ],
        KeyModifiers::SHIFT => [
            (KeyCode::Char('G'), KeyBehavior::GotoEnd),
        ],
        KeyModifiers::CONTROL => [
            (KeyCode::Char('f'), KeyBehavior::PageDown),
            (KeyCode::Char('b'), KeyBehavior::PageUp),

            (KeyCode::Char('d'), KeyBehavior::Quit),
            (KeyCode::Char('c'), KeyBehavior::Quit),
        ],
    }

    dict
}

pub struct UiContext<'b> {
    rx: Arc<ArrayQueue<&'b [u8]>>,
    lines: Vec<&'b [u8]>,
    output: File,
    output_buf: Vec<u8>,
    scroll: usize,
    terminal_size: usize,
    keymap: AHashMap<KeyEvent, KeyBehavior>,
    need_redraw: bool,
    prompt_outdated: bool,
    prompt_state: PromptState,
    prompt: String,
}

impl<'b> UiContext<'b> {
    pub fn new(rx: Arc<ArrayQueue<&'b [u8]>>) -> Result<Self> {
        enable_raw_mode()?;

        let mut output = get_output();

        execute!(output, EnterAlternateScreen, EnableMouseCapture, Hide)?;

        Ok(Self {
            rx,
            lines: Vec::with_capacity(1024),
            scroll: 0,
            output_buf: Vec::with_capacity(1024 * 16),
            terminal_size: crossterm::terminal::size()?.1 as usize - 1,
            keymap: default_keymap(),
            need_redraw: true,
            prompt_state: PromptState::Normal,
            prompt_outdated: true,
            prompt: String::with_capacity(256),
            output,
        })
    }

    fn max_scroll(&self) -> usize {
        self.lines.len().saturating_sub(self.terminal_size)
    }

    pub fn redraw(&mut self) -> Result<()> {
        if self.need_redraw {
            self.output_buf.clear();
            queue!(self.output_buf, Clear(ClearType::All), MoveTo(0, 0))?;

            let end = (self.scroll + self.terminal_size).min(self.lines.len());

            for line in self.lines[self.scroll..end].iter() {
                self.output_buf.extend_from_slice(line);
                self.output_buf.extend_from_slice(b"\r\n");
            }

            self.update_prompt();
            self.output_buf.extend_from_slice(self.prompt.as_bytes());
            self.output.write_all(&self.output_buf)?;
            self.output.flush()?;
            self.need_redraw = false;
        } else if self.prompt_outdated {
            self.update_prompt();
            self.redraw_prompt()?;
        }

        Ok(())
    }

    fn redraw_prompt(&mut self) -> Result<()> {
        self.output_buf.clear();

        let lines = self.terminal_size;
        queue!(
            self.output_buf,
            MoveTo(0, lines as _),
            Clear(ClearType::CurrentLine)
        )?;
        self.output_buf.extend_from_slice(self.prompt.as_bytes());

        self.output.write_all(&self.output_buf)?;
        self.output.flush()?;

        Ok(())
    }

    pub fn push_line(&mut self, line: &'b [u8]) {
        if self.lines.len() < self.terminal_size {
            self.need_redraw = true;
        }
        self.prompt_outdated = true;
        self.lines.push(line);
    }

    fn update_prompt(&mut self) {
        if self.prompt_outdated {
            use std::fmt::Write;
            self.prompt.clear();

            match self.prompt_state {
                PromptState::Normal => {
                    write!(
                        self.prompt,
                        "{}lines {}-{}/{}",
                        SetAttribute(Attribute::Reverse),
                        self.scroll + 1,
                        (self.scroll + self.terminal_size).min(self.lines.len()),
                        self.lines.len(),
                    )
                    .ok();

                    if self.scroll == self.max_scroll() {
                        self.prompt.push_str(" (END)");
                    }

                    write!(self.prompt, "{}", SetAttribute(Attribute::Reset),).ok();
                }
                PromptState::Number(n) => {
                    write!(self.prompt, ":{}", n).ok();
                }
            }

            self.prompt_outdated = false;
        }
    }

    fn goto_scroll(&mut self, idx: usize) {
        self.scroll = idx.min(self.max_scroll());
        self.need_redraw = true;
        self.prompt_outdated = true;
    }

    fn scroll_down(&mut self, idx: usize) {
        self.goto_scroll(self.scroll.saturating_add(idx));
    }

    fn scroll_up(&mut self, idx: usize) {
        self.goto_scroll(self.scroll.saturating_sub(idx));
    }

    pub fn handle_event(&mut self, event: Event) -> Result<bool> {
        match event {
            Event::Mouse(MouseEvent {
                kind: MouseEventKind::ScrollUp,
                ..
            }) => {
                if self.prompt_state == PromptState::Normal {
                    self.scroll_up(1);
                }
            }
            Event::Mouse(MouseEvent {
                kind: MouseEventKind::ScrollDown,
                ..
            }) => {
                if self.prompt_state == PromptState::Normal {
                    self.scroll_down(1);
                }
            }
            Event::Key(ke) => match self.keymap.get(&ke) {
                Some(b) => match b {
                    KeyBehavior::Up => match self.prompt_state.take() {
                        PromptState::Normal => {
                            self.scroll_up(1);
                        }
                        PromptState::Number(n) => {
                            self.scroll_up(n);
                        }
                    },
                    KeyBehavior::Down => match self.prompt_state.take() {
                        PromptState::Normal => {
                            self.scroll_down(1);
                        }
                        PromptState::Number(n) => {
                            self.scroll_down(n);
                        }
                    },
                    KeyBehavior::PageDown => match self.prompt_state.take() {
                        PromptState::Normal => {
                            self.scroll_down(self.terminal_size);
                        }
                        PromptState::Number(n) => {
                            self.scroll_down(self.terminal_size.saturating_mul(n));
                        }
                    },
                    KeyBehavior::PageUp => match self.prompt_state.take() {
                        PromptState::Normal => {
                            self.scroll_up(self.terminal_size);
                        }
                        PromptState::Number(n) => {
                            self.scroll_up(self.terminal_size.saturating_mul(n));
                        }
                    },
                    KeyBehavior::GotoTop => match self.prompt_state.take() {
                        PromptState::Normal => {
                            self.scroll_up(usize::MAX);
                        }
                        PromptState::Number(n) => {
                            self.goto_scroll(n.saturating_sub(1));
                        }
                    },
                    KeyBehavior::GotoEnd => match self.prompt_state.take() {
                        PromptState::Normal => {
                            self.scroll_down(usize::MAX);
                        }
                        PromptState::Number(n) => {
                            self.goto_scroll(n.saturating_sub(1));
                        }
                    },
                    KeyBehavior::Quit => {
                        return Ok(true);
                    }
                },
                None => {
                    if ke.modifiers.is_empty() {
                        if let KeyCode::Char(c @ '0'..='9') = ke.code {
                            let n = (c as u32 - '0' as u32) as usize;
                            match self.prompt_state {
                                PromptState::Normal => {
                                    self.prompt_state = PromptState::Number(n);
                                }
                                PromptState::Number(ref mut pn) => {
                                    *pn = *pn * 10 + n;
                                }
                            }
                            self.prompt_outdated = true;
                        }
                    }
                }
            },
            Event::Resize(_x, y) => {
                self.terminal_size = y as usize - 1;
                self.need_redraw = true;
                self.prompt_outdated = true;
            }
            _ => {}
        };

        Ok(false)
    }

    fn run(&mut self) -> Result<()> {
        static RUN: AtomicBool = AtomicBool::new(true);

        ctrlc::set_handler(|| {
            RUN.store(false, std::sync::atomic::Ordering::SeqCst);
        })
        .expect("Set ctrlc handler");

        const BULK_LINE: usize = 5000;
        const FPS: u64 = 30;
        const TICK: Duration = Duration::from_nanos(Duration::from_secs(1).as_nanos() as u64 / FPS);

        let mut prev_time = Instant::now();

        loop {
            if !RUN.load(std::sync::atomic::Ordering::SeqCst) {
                return Ok(());
            }

            // non blocking
            while poll(Duration::from_nanos(0))? {
                let e = read()?;

                if self.handle_event(e)? {
                    return Ok(());
                }

                self.redraw()?;
            }

            let mut line_count = 0;

            // receive lines max BULK_LINE
            while let Some(line) = self.rx.pop() {
                self.push_line(line);

                line_count += 1;

                if line_count >= BULK_LINE {
                    break;
                }
            }

            self.redraw()?;

            if let Some(sleep) = TICK.checked_sub(prev_time.elapsed()) {
                std::thread::sleep(sleep);
            }

            prev_time = Instant::now();
        }
    }
}

impl<'b> Drop for UiContext<'b> {
    fn drop(&mut self) {
        execute!(self.output, Show, DisableMouseCapture, LeaveAlternateScreen).ok();
        disable_raw_mode().ok();
    }
}

fn main() -> Result<()> {
    #[cfg(feature = "logging")]
    {
        use simplelog::*;
        WriteLogger::init(
            LevelFilter::Trace,
            ConfigBuilder::new().build(),
            File::create("rp.log")?,
        )
        .unwrap();
    }

    let args = match Args::parse() {
        Some(args) => args,
        None => return Ok(()),
    };
    let stdin = get_input(&args)?;
    let rx = Arc::new(ArrayQueue::new(1024 * 16));
    let mut b = Bump::with_capacity(1024 * 1024);

    scope(|s| {
        let tx = rx.clone();
        s.builder()
            .name("stdin".into())
            .spawn(|_| read_from_stdin(stdin, &mut b, tx))?;

        UiContext::new(rx)?.run()?;

        Ok(())
    })
    .unwrap()
}
