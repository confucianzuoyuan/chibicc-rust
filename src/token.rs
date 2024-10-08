use std::fmt::Display;

use crate::position::Pos;

#[derive(Clone, Debug, PartialEq)]
pub enum Tok {
    LeftParen,
    RightParen,
    LeftBrace,
    RightBrace,
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
    Comma,
    Amp,
    Number(i64),
    Ident(String),

    KeywordReturn,
    KeywordIf,
    KeywordElse,
    KeywordFor,
    KeywordWhile,
    KeywordInt,

    EndOfFile,
}

#[derive(Debug, Clone, PartialEq)]
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
                Tok::LeftBrace => "{",
                Tok::RightBrace => "}",
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
                Tok::Comma => ",",
                Tok::Amp => "&",
                Tok::Number(i) => return i.to_string(),
                Tok::Ident(ref ident) => ident,
                Tok::KeywordReturn => "return",
                Tok::KeywordIf => "if",
                Tok::KeywordElse => "else",
                Tok::KeywordFor => "for",
                Tok::KeywordWhile => "while",
                Tok::KeywordInt => "int",
                Tok::EndOfFile => "<eof>",
            };
            string.to_string()
        })();
        write!(f, "{}", string)
    }
}
