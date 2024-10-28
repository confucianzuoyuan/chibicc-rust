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
    PlusEqual,
    PlusPlus,
    Minus,
    MinusEqual,
    MinusMinus,
    MinusGreater,
    Star,
    StarEqual,
    Slash,
    SlashEqual,
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
    AmpEqual,
    AmpAmp,
    Bar,
    BarEqual,
    BarBar,
    Hat,
    HatEqual,
    Dot,
    Tilde,
    Percent,
    PercentEqual,
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
    KeywordBool,
    KeywordEnum,
    KeywordStatic,

    EndOfFile,
}

impl Tok {
    pub fn get_ident_name(&self) -> Option<String> {
        match self {
            Tok::Ident(ref name) => Some(name.clone()),
            _ => None,
        }
    }
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
                Tok::PlusEqual => "+=",
                Tok::PlusPlus => "++",
                Tok::Minus => "-",
                Tok::MinusMinus => "--",
                Tok::MinusEqual => "-=",
                Tok::MinusGreater => "->",
                Tok::Star => "*",
                Tok::StarEqual => "*=",
                Tok::Slash => "/",
                Tok::SlashEqual => "/=",
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
                Tok::AmpEqual => "&=",
                Tok::AmpAmp => "&&",
                Tok::Bar => "|",
                Tok::BarEqual => "|=",
                Tok::BarBar => "||",
                Tok::Hat => "^",
                Tok::HatEqual => "^=",
                Tok::Dot => ".",
                Tok::Tilde => "~",
                Tok::Percent => "%",
                Tok::PercentEqual => "%=",
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
                Tok::KeywordBool => "_Bool",
                Tok::KeywordEnum => "enum",
                Tok::KeywordStatic => "static",
                Tok::EndOfFile => "<eof>",
                Tok::Str(ref string) => return format!("{:?}", string),
            };
            string.to_string()
        })();
        write!(f, "{}", string)
    }
}
