use std::fmt::Display;

use crate::position::Pos;

#[derive(Clone, Debug, PartialEq)]
pub enum Tok {
    LeftParen,
    RightParen,
    Plus,
    Minus,
    Star,
    Slash,
    Equal,
    EqualEqual,
    Bang,
    BangEqual,
    Lesser,
    LesserEqual,
    Greater,
    GreaterEqual,
    Semicolon,
    Number(i64),
    Ident(String),
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
                Tok::LeftParen => "(",
                Tok::RightParen => ")",
                Tok::Plus => "+",
                Tok::Minus => "-",
                Tok::Star => "*",
                Tok::Slash => "/",
                Tok::Equal => "=",
                Tok::EqualEqual => "==",
                Tok::Bang => "!",
                Tok::BangEqual => "!=",
                Tok::Greater => ">",
                Tok::GreaterEqual => ">=",
                Tok::Lesser => "<",
                Tok::LesserEqual => "<=",
                Tok::Semicolon => ";",
                Tok::Number(i) => return i.to_string(),
                Tok::Ident(ref ident) => ident,
                Tok::EndOfFile => "<eof>",
            };
            string.to_string()
        })();
        write!(f, "{}", string)
    }
}
