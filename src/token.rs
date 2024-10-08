use std::fmt::Display;

use crate::position::Pos;

#[derive(Clone, Debug, PartialEq)]
pub enum Tok {
    LEFT_PAREN,
    RIGHT_PAREN,
    Plus,
    Minus,
    Star,
    Slash,
    Number(i64),
    EndOfFile,
}

#[derive(Debug)]
pub struct Token {
    pub pos: Pos,
    pub token: Tok,
}

impl Display for Tok {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let string = (|| {
            let string = match *self {
                Tok::LEFT_PAREN => "(",
                Tok::RIGHT_PAREN => ")",
                Tok::Plus => "+",
                Tok::Minus => "-",
                Tok::Star => "*",
                Tok::Slash => "/",
                Tok::Number(i) => return i.to_string(),
                Tok::EndOfFile => "<eof>",
            };
            string.to_string()
        })();
        write!(f, "{}", string)
    }
}
