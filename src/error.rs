use std::{
    cmp::{max, min},
    fs::File,
    io::{self, Read, Seek, SeekFrom},
};

use crate::{position::Pos, symbol::Symbols, terminal::Terminal, token::Tok};

#[derive(Clone, Debug)]
pub enum Error {
    Msg(String),
    UnexpectedToken {
        expected: String,
        pos: Pos,
        unexpected: Tok,
    },
    InvalidToken {
        pos: Pos,
        start: char,
    },
    Unclosed {
        pos: Pos,
        token: &'static str,
    },
    InvalidEscape {
        escape: String,
        pos: Pos,
    },
    InvalidInitializer {
        pos: Pos,
    },
    Eof,
}

impl Error {
    pub fn show(&self, symbols: &Symbols, terminal: &Terminal) -> io::Result<()> {
        match *self {
            Error::Msg(ref string) => eprintln!("{}", string),
            Error::UnexpectedToken {
                ref expected,
                pos,
                ref unexpected,
            } => {
                eprintln!(
                    "Unexpected token {}, expecting {}{}",
                    unexpected,
                    expected,
                    terminal.end_bold()
                );
                pos.show(symbols, terminal);
                highlight_line(pos, symbols, terminal)?;
            }
            Error::InvalidToken { pos, ref start } => {
                eprintln!(
                    "Unexpected start of token `{}`{}",
                    start,
                    terminal.end_bold()
                );
                pos.show(symbols, terminal);
                highlight_line(pos, symbols, terminal)?;
            }
            Error::Unclosed { pos, token } => {
                eprintln!("Unclosed {}{}", token, terminal.end_bold());
                pos.show(symbols, terminal);
                highlight_line(pos, symbols, terminal)?;
            }
            Error::InvalidEscape { ref escape, pos } => {
                eprintln!("Invalid escape \\{}{}", escape, terminal.end_bold());
                pos.show(symbols, terminal);
                highlight_line(pos, symbols, terminal)?;
            }
            Error::InvalidInitializer { pos } => {
                eprintln!("Invalid Initializer {}", terminal.end_bold());
                pos.show(symbols, terminal);
                highlight_line(pos, symbols, terminal)?;
            }
            Error::Eof => eprintln!("End Of File Error"),
        }
        eprintln!();

        Ok(())
    }
}

impl From<io::Error> for Error {
    fn from(error: io::Error) -> Self {
        Error::Msg(error.to_string())
    }
}

impl<'a> From<&'a Error> for Error {
    fn from(error: &'a Error) -> Self {
        error.clone()
    }
}

fn highlight_line(pos: Pos, symbols: &Symbols, terminal: &Terminal) -> io::Result<()> {
    let filename = symbols.name(pos.file);
    let mut file = File::open(filename)?;
    // TODO: support longer lines.
    const LENGTH: i64 = 4096;
    let mut buffer = [0; LENGTH as usize];
    let start = max(0, pos.byte as i64 - LENGTH / 2);
    file.seek(SeekFrom::Start(start as u64))?;
    let size_read = file.read(&mut buffer)?;
    let buffer = &buffer[..size_read];
    let current_pos = min(pos.byte as usize - start as usize, buffer.len());
    let start_of_line = buffer[..current_pos]
        .iter()
        .rposition(|byte| *byte == b'\n')
        .map(|pos| pos + 1)
        .unwrap_or(0);
    let end_of_line = buffer[current_pos..]
        .iter()
        .position(|byte| *byte == b'\n')
        .map(|pos| pos + current_pos)
        .unwrap_or_else(|| buffer.len());
    let line = &buffer[start_of_line..end_of_line];
    let num_spaces = num_text_size(pos.line as u64);
    let spaces = " ".repeat(num_spaces);
    eprintln!("{}{}{} |", terminal.bold(), terminal.blue(), spaces);
    eprintln!(
        "{} |{}{} {}",
        pos.line,
        terminal.end_bold(),
        terminal.reset_color(),
        String::from_utf8_lossy(line)
    );
    let count = min(pos.column as usize, line.len());
    let spaces_before_hint = " ".repeat(count);
    let hint = "^".repeat(pos.length);
    eprintln!(
        "{}{}{} |{}{}{}{}",
        terminal.bold(),
        terminal.blue(),
        spaces,
        terminal.red(),
        spaces_before_hint,
        hint,
        terminal.reset_color()
    );
    Ok(())
}

pub fn num_text_size(num: u64) -> usize {
    if num == 0 {
        return 1;
    }
    1 + (num as f64).log10().floor() as usize
}
