use clap::Clap;
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
use typed_arena::Arena;

use std::os::unix::prelude::FromRawFd;

#[derive(Clap)]
#[clap(
    name = "rp",
    about = "rust-pager",
    version = "0.1.0",
    author = "Riey <creeper844@gmail.com>"
)]
struct Args {
    #[clap(about = "Open specific file path")]
    path: Option<PathBuf>,
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

fn alloc_slice<'b>(arena: &'b Arena<u8>, slice: &[u8]) -> &'b [u8] {
    unsafe {
        let uninit_line = arena.alloc_uninitialized(slice.len());
        std::ptr::copy_nonoverlapping(
            slice.as_ptr(),
            uninit_line.as_mut_ptr() as *mut u8,
            slice.len(),
        );
        std::mem::transmute(uninit_line)
    }
}

fn alloc_slice_sub1<'b>(arena: &'b Arena<u8>, slice: &[u8]) -> &'b [u8] {
    unsafe {
        match slice.len().checked_sub(1) {
            Some(len) => {
                let uninit_line = arena.alloc_uninitialized(len);
                std::ptr::copy_nonoverlapping(
                    slice.as_ptr(),
                    uninit_line.as_mut_ptr() as *mut u8,
                    len,
                );
                std::mem::transmute(uninit_line)
            }
            None => &[],
        }
    }
}

fn push_newline<'b>(arena: &'b Arena<u8>, tx: &ArrayQueue<&'b [u8]>, line: &[u8]) {
    let line = alloc_slice_sub1(arena, line);

    while tx.push(line).is_err() {
        std::thread::sleep(Duration::from_millis(50));
    }
}

fn push_no_newline<'b>(arena: &'b Arena<u8>, tx: &ArrayQueue<&'b [u8]>, line: &[u8]) {
    let line = alloc_slice(arena, line);

    while tx.push(line).is_err() {
        std::thread::sleep(Duration::from_millis(50));
    }
}

fn read_from_stdin<'b>(
    mut stdin: File,
    arena: &'b mut Arena<u8>,
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
                push_no_newline(arena, &tx, buf);
            }
            break Ok(());
        }

        start_len = buf.len();

        loop {
            match memchr::memchr(b'\n', buf) {
                Some(pos) => {
                    let (line, new_buf) = buf.split_at(pos + 1);
                    buf = new_buf;
                    push_newline(arena, &tx, line);
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
            push_no_newline(arena, &tx, &buf);
            break Ok(());
        } else if start_len == end_len {
            // no bytes processed
        } else {
            stdin_buf.copy_within((start_len - end_len)..start_len, 0);
            start_len = end_len;
        }
    }
}

pub struct UiContext<'b> {
    rx: Arc<ArrayQueue<&'b [u8]>>,
    lines: Vec<&'b [u8]>,
    output: File,
    output_buf: Vec<u8>,
    scroll: usize,
    terminal_size: usize,
    need_redraw: bool,
    prompt_outdated: bool,
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
            need_redraw: true,
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

            self.prompt_outdated = false;
        }
    }

    fn scroll_down(&mut self, idx: usize) {
        self.scroll = (self.scroll.saturating_add(idx)).min(self.max_scroll());
        self.need_redraw = true;
        self.prompt_outdated = true;
    }

    fn scroll_up(&mut self, idx: usize) {
        self.scroll = self.scroll.saturating_sub(idx);
        self.need_redraw = true;
        self.prompt_outdated = true;
    }

    pub fn handle_event(&mut self, event: Event) -> Result<bool> {
        match event {
            Event::Key(KeyEvent {
                code: KeyCode::Char('c'),
                modifiers: KeyModifiers::CONTROL,
            })
            | Event::Key(KeyEvent {
                code: KeyCode::Char('d'),
                modifiers: KeyModifiers::CONTROL,
            })
            | Event::Key(KeyEvent {
                code: KeyCode::Char('q'),
                modifiers: KeyModifiers::NONE,
            }) => {
                return Ok(true);
            }
            Event::Mouse(MouseEvent {
                kind: MouseEventKind::ScrollUp,
                ..
            })
            | Event::Key(KeyEvent {
                code: KeyCode::Char('k'),
                modifiers: KeyModifiers::NONE,
            }) => {
                self.scroll_up(1);
            }
            Event::Mouse(MouseEvent {
                kind: MouseEventKind::ScrollDown,
                ..
            })
            | Event::Key(KeyEvent {
                code: KeyCode::Char('j'),
                modifiers: KeyModifiers::NONE,
            }) => {
                self.scroll_down(1);
            }
            Event::Key(KeyEvent {
                code: KeyCode::Char('G'),
                modifiers: KeyModifiers::SHIFT,
            }) => {
                // G
                self.scroll_down(usize::MAX);
            }
            Event::Key(KeyEvent {
                code: KeyCode::Char('g'),
                modifiers: KeyModifiers::NONE,
            }) => {
                // g
                self.scroll_up(usize::MAX);
            }
            Event::Key(KeyEvent {
                code: KeyCode::PageUp,
                ..
            })
            | Event::Key(KeyEvent {
                code: KeyCode::Char('b'),
                modifiers: KeyModifiers::CONTROL,
            }) => {
                self.scroll_up(self.terminal_size);
            }
            Event::Key(KeyEvent {
                code: KeyCode::PageDown,
                ..
            })
            | Event::Key(KeyEvent {
                code: KeyCode::Char('f'),
                modifiers: KeyModifiers::CONTROL,
            }) => {
                self.scroll_down(self.terminal_size);
            }
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
                if line_count >= BULK_LINE {
                    break;
                }

                self.push_line(line);
                line_count += 1;
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
    #[cfg(feature = "logging")] {
        use simplelog::*;
        WriteLogger::init(LevelFilter::Trace, ConfigBuilder::new().build(), File::create("rp.log")?).unwrap();
    }

    let args = Args::parse();
    let stdin = get_input(&args)?;
    let rx = Arc::new(ArrayQueue::new(1024 * 16));
    let mut arena = Arena::with_capacity(1024 * 1024);

    scope(|s| {
        let tx = rx.clone();
        s.builder()
            .name("stdin".into())
            .spawn(|_| read_from_stdin(stdin, &mut arena, tx))?;

        UiContext::new(rx)?.run()?;

        Ok(())
    })
    .unwrap()
}
