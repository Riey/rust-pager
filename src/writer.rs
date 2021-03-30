use ahash::AHashMap;
use crossbeam_queue::ArrayQueue;
use crossterm::{
    cursor::{Hide, MoveTo, MoveToNextLine, Show},
    event::{
        poll, read, DisableMouseCapture, EnableMouseCapture, Event, KeyCode, KeyEvent,
        KeyModifiers, MouseEvent, MouseEventKind,
    },
    execute, queue,
    style::{
        Attribute, Attributes, Color, SetAttribute, SetAttributes, SetBackgroundColor,
        SetForegroundColor,
    },
    terminal::{
        disable_raw_mode, enable_raw_mode, Clear, ClearType, DisableLineWrap, EnableLineWrap,
        EnterAlternateScreen, LeaveAlternateScreen,
    },
    Result,
};
use rayon::prelude::*;
use smallvec::SmallVec;
use std::{
    fs::File,
    io::Write,
    sync::atomic::Ordering,
    sync::Arc,
    time::{Duration, Instant},
};

use crate::shared::{RpChar, RpLine};

type SearchPositionArr = SmallVec<[SearchPosition; 4]>;
const OUTBUF_SIZE: usize = 1024 * 20;

fn get_output() -> File {
    File::create("/dev/tty").expect("Can't open tty")
}

#[derive(Clone, Copy)]
pub struct SearchPosition {
    start: u32,
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
pub enum ScrollSize {
    One,
    HalfPage,
    Page,
    End,
}

impl ScrollSize {
    pub const fn calculate(self, terminal_line: usize) -> usize {
        match self {
            Self::One => 1,
            Self::HalfPage => terminal_line / 2,
            Self::Page => terminal_line,
            Self::End => usize::MAX,
        }
    }
}

#[derive(Clone, Copy)]
pub enum KeyBehavior {
    Quit,

    Down(ScrollSize),
    Up(ScrollSize),

    SearchNext,
    SearchPrev,

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
            (KeyCode::Enter, KeyBehavior::Down(ScrollSize::One)),
            (KeyCode::Down, KeyBehavior::Down(ScrollSize::One)),
            (KeyCode::Char('j'), KeyBehavior::Down(ScrollSize::One)),

            (KeyCode::Up, KeyBehavior::Up(ScrollSize::One)),
            (KeyCode::Char('k'), KeyBehavior::Up(ScrollSize::One)),

            (KeyCode::Char('u'), KeyBehavior::Up(ScrollSize::HalfPage)),
            (KeyCode::Char('d'), KeyBehavior::Down(ScrollSize::HalfPage)),
            (KeyCode::Left, KeyBehavior::Up(ScrollSize::HalfPage)),
            (KeyCode::Right, KeyBehavior::Down(ScrollSize::HalfPage)),

            (KeyCode::Char('f'), KeyBehavior::Down(ScrollSize::Page)),
            (KeyCode::Char(' '), KeyBehavior::Down(ScrollSize::Page)),
            (KeyCode::Char('b'), KeyBehavior::Up(ScrollSize::Page)),
            (KeyCode::PageDown, KeyBehavior::Down(ScrollSize::Page)),
            (KeyCode::PageUp, KeyBehavior::Up(ScrollSize::Page)),

            (KeyCode::Esc, KeyBehavior::NormalMode),
            (KeyCode::Home, KeyBehavior::Up(ScrollSize::End)),
            (KeyCode::End, KeyBehavior::Down(ScrollSize::End)),
            (KeyCode::Char('g'), KeyBehavior::Up(ScrollSize::End)),

            (KeyCode::Char('q'), KeyBehavior::Quit),

            (KeyCode::Char('/'), KeyBehavior::Search),
            (KeyCode::Char('n'), KeyBehavior::SearchNext),

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
            (KeyCode::Char('G'), KeyBehavior::Down(ScrollSize::End)),
            (KeyCode::Char('N'), KeyBehavior::SearchPrev),
            (KeyCode::Char('Q'), KeyBehavior::Quit),
        ],
        KeyModifiers::CONTROL => [
            (KeyCode::Char('u'), KeyBehavior::Up(ScrollSize::HalfPage)),
            (KeyCode::Char('d'), KeyBehavior::Down(ScrollSize::HalfPage)),
            (KeyCode::Char('f'), KeyBehavior::Down(ScrollSize::Page)),
            (KeyCode::Char('v'), KeyBehavior::Down(ScrollSize::Page)),
            (KeyCode::Char('b'), KeyBehavior::Up(ScrollSize::Page)),

            (KeyCode::Char('e'), KeyBehavior::Down(ScrollSize::One)),
            (KeyCode::Char('n'), KeyBehavior::Down(ScrollSize::One)),

            (KeyCode::Char('y'), KeyBehavior::Up(ScrollSize::One)),
            (KeyCode::Char('k'), KeyBehavior::Up(ScrollSize::One)),
            (KeyCode::Char('p'), KeyBehavior::Up(ScrollSize::One)),

            (KeyCode::Char('d'), KeyBehavior::Quit),
            (KeyCode::Char('c'), KeyBehavior::Quit),
        ],
    }

    dict
}

pub struct UiContext<'b> {
    rx: Arc<ArrayQueue<RpLine<'b>>>,
    lines: Vec<RpLine<'b>>,
    search_positions: Vec<SearchPositionArr>,
    search_char_len: usize,
    output: File,
    output_buf: Vec<u8>,
    scroll: usize,
    terminal_line: usize,
    keymap: AHashMap<KeyEvent, KeyBehavior>,
    need_redraw: bool,
    prompt_outdated: bool,
    prompt_state: PromptState,
    prompt: String,
}

impl<'b> UiContext<'b> {
    pub fn new(rx: Arc<ArrayQueue<RpLine<'b>>>) -> Result<Self> {
        enable_raw_mode()?;

        let mut output = get_output();

        execute!(
            output,
            EnterAlternateScreen,
            EnableMouseCapture,
            DisableLineWrap,
            Hide
        )?;

        let (_x, y) = crossterm::terminal::size()?;

        Ok(Self {
            rx,
            lines: Vec::with_capacity(1024),
            scroll: 0,
            output_buf: vec![0; OUTBUF_SIZE],
            search_positions: Vec::new(),
            search_char_len: 0,
            terminal_line: y as usize - 1,
            keymap: default_keymap(),
            need_redraw: true,
            prompt_state: PromptState::Normal,
            prompt_outdated: true,
            prompt: String::with_capacity(256),
            output,
        })
    }

    fn max_scroll(&self) -> usize {
        self.lines.len().saturating_sub(self.terminal_line)
    }

    pub fn redraw(&mut self) -> Result<()> {
        if self.need_redraw {
            #[cfg(feature = "logging")]
            log::debug!("REDRAW");

            self.output_buf.clear();

            queue!(self.output_buf, MoveTo(0, 0))?;

            let mut ch_writer = ChWriter::new();
            let end = (self.scroll + self.terminal_line).min(self.lines.len());

            if self.search_positions.is_empty() {
                for line in self.lines[self.scroll..end].iter() {
                    queue!(self.output_buf, Clear(ClearType::CurrentLine))?;
                    ch_writer.write_slice(&mut self.output_buf, line)?;
                    queue!(self.output_buf, MoveToNextLine(1))?;
                }
            } else {
                for (line, search) in self.lines[self.scroll..end]
                    .iter()
                    .zip(self.search_positions[self.scroll..end].iter())
                {
                    queue!(self.output_buf, Clear(ClearType::CurrentLine))?;
                    let mut prev_pos = 0;
                    for pos in search.iter() {
                        let start = pos.start as usize;
                        let end = start + self.search_char_len;

                        ch_writer.write_slice(&mut self.output_buf, &line[prev_pos..start])?;
                        ch_writer.write_slice_reverse(&mut self.output_buf, &line[start..end])?;
                        prev_pos = end;
                    }
                    ch_writer.write_slice(&mut self.output_buf, &line[prev_pos..])?;
                    queue!(self.output_buf, MoveToNextLine(1))?;
                }
            }

            queue!(self.output_buf, SetAttribute(Attribute::Reset),)?;
            self.update_prompt();
            queue!(self.output_buf, Clear(ClearType::CurrentLine))?;
            self.output_buf.extend_from_slice(self.prompt.as_bytes());
            #[cfg(feature = "logging")]
            log::trace!("Write {} bytes", self.output_buf.len());
            self.output.write(&self.output_buf)?;
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

        let lines = self.terminal_line;
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

    pub fn push_line(&mut self, line: RpLine<'b>) {
        if self.lines.len() < self.terminal_line {
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
                        (self.scroll + self.terminal_line).min(self.lines.len()),
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
        let new_scroll = idx.min(self.max_scroll());
        if new_scroll != self.scroll {
            self.scroll = new_scroll;
            self.need_redraw = true;
            self.prompt_outdated = true;
        }
    }

    fn scroll_down(&mut self, idx: usize) {
        self.goto_scroll(self.scroll.saturating_add(idx));
    }

    fn scroll_up(&mut self, idx: usize) {
        self.goto_scroll(self.scroll.saturating_sub(idx));
    }

    fn move_search(&mut self, forward: bool) {
        let next = self.search_positions[self.scroll..]
            .iter()
            .enumerate()
            .skip(1)
            .map(|(i, p)| (i + self.scroll, p));

        let prev = self.search_positions[0..self.scroll].iter().enumerate();

        let line = if forward {
            next.chain(prev)
                .find_map(|(line, p)| if !p.is_empty() { Some(line) } else { None })
        } else {
            prev.rev()
                .chain(next.rev())
                .find_map(|(line, p)| if !p.is_empty() { Some(line) } else { None })
        };

        if let Some(line) = line {
            self.goto_scroll(line);
        }
    }

    fn search(&mut self, needle: &str) {
        if !self.search_positions.is_empty() {
            self.need_redraw = true;
            self.search_positions.clear();
        }

        if needle.is_empty() {
            return;
        }

        let char_count = needle.chars().count();
        self.search_char_len = char_count;

        #[cfg(feature = "logging")]
        log::debug!("Search: {:?}", needle);

        self.need_redraw = true;

        self.lines
            .par_iter()
            .map(|chars| {
                let mut arr = SearchPositionArr::new();

                for i in 0..chars.len() {
                    if chars[i..]
                        .iter()
                        .take(char_count)
                        .map(|c| c.ch)
                        .eq(needle.chars())
                    {
                        arr.push(SearchPosition { start: i as u32 });
                    }
                }

                arr
            })
            .collect_into_vec(&mut self.search_positions);

        self.move_search(true);
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
                                return Ok(false);
                            }
                            KeyCode::Backspace => {
                                if s.pop().is_none() {
                                    self.prompt_state = PromptState::Normal;
                                }

                                self.prompt_outdated = true;
                                return Ok(false);
                            }
                            KeyCode::Enter => {
                                let needle = std::mem::take(s);
                                self.search(&needle);
                                self.prompt_state = PromptState::Normal;
                                self.prompt_outdated = true;
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
                            self.search("");
                            self.prompt_outdated = true;
                        }
                        KeyBehavior::Search => {
                            self.prompt_state = PromptState::Search(String::new());
                            self.prompt_outdated = true;
                        }
                        KeyBehavior::SearchNext => {
                            self.move_search(true);
                        }
                        KeyBehavior::SearchPrev => {
                            self.move_search(false);
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
                        KeyBehavior::Up(size) => {
                            let size = size.calculate(self.terminal_line);
                            let n = match self.prompt_state.take() {
                                PromptState::Number(n) => n,
                                _ => 1,
                            };
                            self.scroll_up(size.wrapping_mul(n));
                        }
                        KeyBehavior::Down(size) => {
                            let size = size.calculate(self.terminal_line);
                            let n = match self.prompt_state.take() {
                                PromptState::Number(n) => n,
                                _ => 1,
                            };
                            self.scroll_down(size.wrapping_mul(n));
                        }
                        KeyBehavior::Quit => {
                            return Ok(true);
                        }
                    },
                    None => {}
                }
            }
            Event::Resize(_x, y) => {
                self.terminal_line = y as usize - 1;
                self.need_redraw = true;
                self.prompt_outdated = true;
            }
            _ => {}
        };

        Ok(false)
    }

    pub fn run(&mut self) -> Result<()> {
        const BULK_LINE: usize = 5000;
        const FPS: u64 = 30;
        const TICK: Duration = Duration::from_nanos(Duration::from_secs(1).as_nanos() as u64 / FPS);

        let mut prev_time = Instant::now();

        loop {
            if !crate::RUN.load(Ordering::Acquire) {
                return Ok(());
            }

            // non blocking
            while poll(Duration::from_nanos(0))? {
                let e = read()?;

                if self.handle_event(e)? {
                    return Ok(());
                }
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
        execute!(
            self.output,
            Show,
            EnableLineWrap,
            DisableMouseCapture,
            LeaveAlternateScreen
        )
        .ok();
        disable_raw_mode().ok();
    }
}

struct ChWriter {
    current_color: Color,
    current_bgcolor: Color,
    current_attribute: Attributes,
}

impl ChWriter {
    pub fn new() -> Self {
        Self {
            current_color: Color::Reset,
            current_bgcolor: Color::Reset,
            current_attribute: Attributes::default(),
        }
    }

    pub fn write_slice_reverse(&mut self, out: &mut Vec<u8>, chars: &[RpChar]) -> Result<()> {
        chars.iter().copied().try_for_each(|mut ch| {
            ch.attribute.set(Attribute::Reverse);
            self.write(out, ch)
        })?;
        queue!(out, SetAttribute(Attribute::NoReverse))
    }

    pub fn write_slice(&mut self, out: &mut Vec<u8>, chars: &[RpChar]) -> Result<()> {
        chars.iter().copied().try_for_each(|ch| self.write(out, ch))
    }

    pub fn write(&mut self, out: &mut Vec<u8>, ch: RpChar) -> Result<()> {
        if self.current_attribute != ch.attribute {
            queue!(out, SetAttributes(ch.attribute))?;
            // Reset attribute also reset colors
            if ch.attribute.has(Attribute::Reset) {
                self.current_color = Color::Reset;
                self.current_bgcolor = Color::Reset;
            }
            self.current_attribute = ch.attribute;
        }
        if ch.foreground != self.current_color {
            queue!(out, SetForegroundColor(ch.foreground))?;
            self.current_color = ch.foreground;
        }
        if ch.background != self.current_bgcolor {
            queue!(out, SetBackgroundColor(ch.background))?;
            self.current_bgcolor = ch.background;
        }

        write!(out, "{}", ch.ch)?;

        Ok(())
    }
}
