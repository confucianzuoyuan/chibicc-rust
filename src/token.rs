use std::fmt::Display;

use crate::position::Pos;

#[derive(Clone, Debug, PartialEq)]
pub enum Tok {
    LeftParen,
    RightParen,
    LeftBrace,
    RightBrace,
    LeftBracket,
    RightBracket,
    Plus,
    Minus,
    MinusGreater,
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
    Dot,
    Number(i64),
    Ident(String),
    Str(String),

    KeywordReturn,
    KeywordIf,
    KeywordElse,
    KeywordFor,
    KeywordWhile,
    KeywordInt,
    KeywordChar,
    KeywordLong,
    KeywordShort,
    KeywordSizeof,
    KeywordStruct,
    KeywordUnion,
    KeywordVoid,
    KeywordTypedef,

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
                Tok::LeftBracket => "[",
                Tok::RightBracket => "]",
                Tok::Plus => "+",
                Tok::Minus => "-",
                Tok::MinusGreater => "->",
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
                Tok::Dot => ".",
                Tok::Number(i) => return i.to_string(),
                Tok::Ident(ref ident) => ident,
                Tok::KeywordReturn => "return",
                Tok::KeywordIf => "if",
                Tok::KeywordElse => "else",
                Tok::KeywordFor => "for",
                Tok::KeywordWhile => "while",
                Tok::KeywordInt => "int",
                Tok::KeywordChar => "char",
                Tok::KeywordLong => "long",
                Tok::KeywordShort => "short",
                Tok::KeywordSizeof => "sizeof",
                Tok::KeywordStruct => "struct",
                Tok::KeywordUnion => "union",
                Tok::KeywordVoid => "void",
                Tok::KeywordTypedef => "typedef",
                Tok::EndOfFile => "<eof>",
                Tok::Str(ref string) => return format!("{:?}", string),
            };
            string.to_string()
        })();
        write!(f, "{}", string)
    }
}
