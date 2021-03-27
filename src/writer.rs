use ahash::AHashMap;
use crossbeam_queue::ArrayQueue;
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
    Result,
};
use rayon::prelude::*;
use smallvec::SmallVec;
use std::{
    fs::File,
    io::Write,
    sync::atomic::AtomicBool,
    sync::Arc,
    time::{Duration, Instant},
};

type SearchPositionArr = SmallVec<[SearchPosition; 8]>;

fn get_output() -> File {
    use std::os::unix::prelude::FromRawFd;
    unsafe { File::from_raw_fd(libc::STDOUT_FILENO) }
}

#[derive(Clone, Copy)]
pub struct SearchPosition {
    start: u32,
    end: u32,
}

#[derive(Clone, PartialEq, Eq)]
pub enum PromptState {
    Normal,
    Number(usize),
    Search(String),
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

    NormalMode,
    Number(u32),
    Search,
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

            (KeyCode::Esc, KeyBehavior::NormalMode),
            (KeyCode::Home, KeyBehavior::GotoTop),
            (KeyCode::End, KeyBehavior::GotoEnd),
            (KeyCode::Char('g'), KeyBehavior::GotoTop),

            (KeyCode::Char('q'), KeyBehavior::Quit),

            (KeyCode::Char('/'), KeyBehavior::Search),
            (KeyCode::Char('0'), KeyBehavior::Number(0)),
            (KeyCode::Char('1'), KeyBehavior::Number(1)),
            (KeyCode::Char('2'), KeyBehavior::Number(2)),
            (KeyCode::Char('3'), KeyBehavior::Number(3)),
            (KeyCode::Char('4'), KeyBehavior::Number(4)),
            (KeyCode::Char('5'), KeyBehavior::Number(5)),
            (KeyCode::Char('6'), KeyBehavior::Number(6)),
            (KeyCode::Char('7'), KeyBehavior::Number(7)),
            (KeyCode::Char('8'), KeyBehavior::Number(8)),
            (KeyCode::Char('9'), KeyBehavior::Number(9)),
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
    search_positions: Vec<SearchPositionArr>,
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
            search_positions: Vec::new(),
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
            #[cfg(feature = "logging")]
            log::debug!("REDRAW");

            self.output_buf.clear();
            queue!(self.output_buf, Clear(ClearType::All), MoveTo(0, 0))?;

            let end = (self.scroll + self.terminal_size).min(self.lines.len());

            if self.search_positions.is_empty() {
                for line in self.lines[self.scroll..end].iter() {
                    self.output_buf.extend_from_slice(line);
                    self.output_buf.extend_from_slice(b"\r\n");
                }
            } else {
                for (line, search) in self.lines[self.scroll..end]
                    .iter()
                    .zip(self.search_positions[self.scroll..end].iter())
                {
                    let mut prev_pos = 0;
                    for pos in search.iter() {
                        self.output_buf
                            .extend_from_slice(&line[prev_pos as usize..pos.start as usize]);
                        queue!(self.output_buf, SetAttribute(Attribute::Reverse))?;
                        self.output_buf
                            .extend_from_slice(&line[pos.start as usize..pos.end as usize]);
                        queue!(self.output_buf, SetAttribute(Attribute::Reset))?;
                        prev_pos = pos.end;
                    }
                    self.output_buf
                        .extend_from_slice(&line[prev_pos as usize..]);
                    self.output_buf.extend_from_slice(b"\r\n");
                }
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
                PromptState::Search(ref s) => {
                    write!(
                        self.prompt,
                        "{}/{}{}",
                        SetAttribute(Attribute::Reverse),
                        s,
                        SetAttribute(Attribute::Reset),
                    )
                    .ok();
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

    fn search(&mut self, needle: &str) {
        self.search_positions.clear();
        if needle.is_empty() {
            return;
        }

        #[cfg(feature = "logging")]
        log::debug!("Search: {:?}", needle);

        self.lines
            .par_iter()
            .map(|bytes| {
                let mut v = SearchPositionArr::new();
                let mut prev_pos = 0;

                while let Some(pos) = twoway::find_bytes(&bytes[prev_pos..], needle.as_bytes()) {
                    v.push(SearchPosition {
                        start: pos as u32,
                        end: (pos + needle.len()) as u32,
                    });
                    prev_pos = pos + needle.len();
                }

                v
            })
            .collect_into_vec(&mut self.search_positions);
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
            Event::Key(ke) => {
                if let PromptState::Search(ref mut s) = self.prompt_state {
                    if !ke
                        .modifiers
                        .intersects(KeyModifiers::CONTROL | KeyModifiers::ALT)
                    {
                        match ke.code {
                            KeyCode::Char(c) => {
                                s.push(c);
                                self.prompt_outdated = true;
                                self.need_redraw = true;
                                return Ok(false);
                            }
                            KeyCode::Enter => {
                                let needle = std::mem::take(s);
                                self.search(&needle);
                                self.prompt_state = PromptState::Normal;
                                self.prompt_outdated = true;
                                self.need_redraw = true;
                                return Ok(false);
                            }
                            _ => {}
                        }
                    }
                }

                match self.keymap.get(&ke) {
                    Some(b) => match b {
                        KeyBehavior::NormalMode => {
                            self.prompt_state.take();
                            self.prompt_outdated = true;
                        }
                        KeyBehavior::Search => {
                            self.prompt_state = PromptState::Search(String::new());
                            self.prompt_outdated = true;
                        }
                        KeyBehavior::Number(n) => match self.prompt_state {
                            PromptState::Number(ref mut pn) => {
                                *pn = *pn * 10 + (*n as usize);
                                self.prompt_outdated = true;
                            }
                            _ => {
                                self.prompt_state = PromptState::Number(*n as usize);
                                self.prompt_outdated = true;
                            }
                        },
                        KeyBehavior::Up => match self.prompt_state.take() {
                            PromptState::Number(n) => {
                                self.scroll_up(n);
                            }
                            _ => {
                                self.scroll_up(1);
                            }
                        },
                        KeyBehavior::Down => match self.prompt_state.take() {
                            PromptState::Number(n) => {
                                self.scroll_down(n);
                            }
                            _ => {
                                self.scroll_down(1);
                            }
                        },
                        KeyBehavior::PageDown => match self.prompt_state.take() {
                            PromptState::Number(n) => {
                                self.scroll_down(self.terminal_size.saturating_mul(n));
                            }
                            _ => {
                                self.scroll_down(self.terminal_size);
                            }
                        },
                        KeyBehavior::PageUp => match self.prompt_state.take() {
                            PromptState::Number(n) => {
                                self.scroll_up(self.terminal_size.saturating_mul(n));
                            }
                            _ => {
                                self.scroll_up(self.terminal_size);
                            }
                        },
                        KeyBehavior::GotoTop => match self.prompt_state.take() {
                            PromptState::Number(n) => {
                                self.goto_scroll(n.saturating_sub(1));
                            }
                            _ => {
                                self.scroll_up(usize::MAX);
                            }
                        },
                        KeyBehavior::GotoEnd => match self.prompt_state.take() {
                            PromptState::Number(n) => {
                                self.goto_scroll(n.saturating_sub(1));
                            }
                            _ => {
                                self.scroll_down(usize::MAX);
                            }
                        },
                        KeyBehavior::Quit => {
                            return Ok(true);
                        }
                    },
                    None => {}
                }
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

    pub fn run(&mut self) -> Result<()> {
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
