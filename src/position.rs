use std::fmt::{self, Debug, Display, Formatter};
use std::u32;

use crate::symbol::{Symbol, Symbols};
use crate::terminal::Terminal;

#[derive(Clone, Copy, Debug, PartialEq)]
pub struct Pos {
    pub byte: u64,
    pub column: u32,
    pub file: Symbol,
    pub length: usize,
    pub line: u32,
}

impl Pos {
    pub fn new(line: u32, column: u32, byte: u64, file: Symbol, length: usize) -> Self {
        Pos {
            byte,
            column,
            file,
            length,
            line,
        }
    }

    pub fn grow(&self, pos: Pos) -> Self {
        Pos {
            byte: self.byte,
            column: self.column,
            file: self.file,
            length: (pos.byte - self.byte) as usize + pos.length,
            line: self.line,
        }
    }

    pub fn show(&self, symbols: &Symbols, terminal: &Terminal) {
        let filename = symbols.name(self.file);
        eprintln!(
            "   {}{}-->{}{} {}:{}:{}",
            terminal.bold(),
            terminal.blue(),
            terminal.reset_color(),
            terminal.end_bold(),
            filename,
            self.line,
            self.column
        )
    }
}

impl Display for Pos {
    fn fmt(&self, formatter: &mut Formatter) -> fmt::Result {
        write!(formatter, "{}:{}:", self.line, self.column)
    }
}

#[derive(Debug, Clone)]
pub struct WithPos<T> {
    pub node: T,
    pub pos: Pos,
}

impl<T> WithPos<T> {
    pub fn new(node: T, pos: Pos) -> Self {
        Self { node, pos }
    }
}

impl<T: PartialEq> PartialEq for WithPos<T> {
    fn eq(&self, other: &Self) -> bool {
        self.node == other.node
    }
}
