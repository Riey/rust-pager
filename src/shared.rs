use crossterm::style::{Attribute, Attributes, Color};
use std::convert::TryFrom;
use vte::Params;

#[derive(Clone, Copy)]
pub struct RpChar {
    pub ch: char,
    pub foreground: Color,
    pub background: Color,
    pub attribute: Attributes,
}

pub type RpLine<'b> = &'b [RpChar];

pub struct Buffer {
    pub buf: Vec<RpChar>,
    pub foreground: Color,
    pub background: Color,
    pub attribute: Attributes,
}

impl Buffer {
    pub fn new() -> Self {
        Self {
            buf: Vec::with_capacity(64),
            foreground: Color::Reset,
            background: Color::Reset,
            attribute: Attributes::default(),
        }
    }

    // Copied from vt100
    fn sgr(&mut self, params: &Params) {
        if params.is_empty() {
            self.attribute = Attributes::default();
            return;
        }

        let mut iter = params.iter();

        macro_rules! next_param {
            () => {
                match iter.next() {
                    Some(n) => n,
                    _ => return,
                }
            };
        }

        macro_rules! to_u8 {
            ($n:expr) => {
                if let Some(n) = u8::try_from($n).ok() {
                    n
                } else {
                    return;
                }
            };
        }

        macro_rules! next_param_u8 {
            () => {
                if let &[n] = next_param!() {
                    to_u8!(n)
                } else {
                    return;
                }
            };
        }

        loop {
            match next_param!() {
                &[0] => self.attribute = Attribute::Reset.into(),
                &[1] => self.attribute.set(Attribute::Bold),
                &[3] => self.attribute.set(Attribute::Italic),
                &[4] => self.attribute.set(Attribute::Underlined),
                &[7] => self.attribute.set(Attribute::Reverse),
                &[22] => self.attribute.set(Attribute::NoBold),
                &[23] => self.attribute.set(Attribute::NoItalic),
                &[24] => self.attribute.set(Attribute::NoUnderline),
                &[27] => self.attribute.set(Attribute::NoReverse),
                &[n] if (30..=37).contains(&n) => {
                    self.foreground = Color::AnsiValue(to_u8!(n) - 30);
                }
                &[38, 2, r, g, b] => {
                    self.foreground = Color::Rgb {
                        r: to_u8!(r),
                        g: to_u8!(g),
                        b: to_u8!(b),
                    };
                }
                &[38, 5, i] => {
                    self.foreground = idx_color(to_u8!(i));
                }
                &[38] => match next_param!() {
                    &[2] => {
                        let r = next_param_u8!();
                        let g = next_param_u8!();
                        let b = next_param_u8!();
                        self.foreground = Color::Rgb { r, g, b };
                    }
                    &[5] => {
                        self.foreground = idx_color(next_param_u8!());
                    }
                    _ => {}
                },
                &[39] => {
                    self.foreground = Color::Reset;
                }
                &[n] if (40..=47).contains(&n) => {
                    self.background = idx_color(to_u8!(n) - 40);
                }
                &[48, 2, r, g, b] => {
                    self.background = Color::Rgb {
                        r: to_u8!(r),
                        g: to_u8!(g),
                        b: to_u8!(b),
                    };
                }
                &[48, 5, i] => {
                    self.background = idx_color(to_u8!(i));
                }
                &[48] => match next_param!() {
                    &[2] => {
                        let r = next_param_u8!();
                        let g = next_param_u8!();
                        let b = next_param_u8!();
                        self.background = Color::Rgb { r, g, b };
                    }
                    &[5] => {
                        self.background = idx_color(next_param_u8!());
                    }
                    _ => {}
                },
                &[49] => {
                    self.background = Color::Reset;
                }
                &[n] if (90..=97).contains(&n) => {
                    self.foreground = idx_color(to_u8!(n) - 82);
                }
                &[n] if (100..=107).contains(&n) => {
                    self.background = idx_color(to_u8!(n) - 92);
                }
                _ => {}
            }
        }
    }
}

const fn idx_color(c: u8) -> Color {
    use Color::*;
    match c {
        0 => Black,
        1 => DarkRed,
        2 => DarkGreen,
        3 => DarkYellow,
        4 => DarkBlue,
        5 => DarkMagenta,
        6 => DarkCyan,
        7 => Grey,
        8 => DarkGrey,
        9 => Red,
        10 => Green,
        11 => Yellow,
        12 => Blue,
        13 => Magenta,
        14 => Cyan,
        15 => White,
        n => Color::AnsiValue(n),
    }
}

impl vte::Perform for Buffer {
    fn print(&mut self, ch: char) {
        self.buf.push(RpChar {
            ch,
            foreground: self.foreground,
            background: self.background,
            attribute: self.attribute,
        });
    }

    fn csi_dispatch(&mut self, params: &Params, intermediates: &[u8], _ignore: bool, action: char) {
        match intermediates {
            &[] => match action {
                'm' => {
                    self.sgr(params);
                }
                _ => {}
            },
            _ => {}
        }
    }
}
