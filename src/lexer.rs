use std::{
    io::{Bytes, Read},
    iter::Peekable,
};

use crate::{
    error::{num_text_size, Error},
    position::Pos,
    symbol::Symbol,
    token::{Tok, Token},
};

pub type Result<T> = std::result::Result<T, Error>;

pub struct Lexer<R: Read> {
    bytes_iter: Peekable<Bytes<R>>,
    pos: Pos,
    saved_pos: Pos,
}

impl<R: Read> Lexer<R> {
    pub fn new(reader: R, filename: Symbol) -> Self {
        Lexer {
            bytes_iter: reader.bytes().peekable(),
            pos: Pos::new(1, 1, 0, filename, 0),
            saved_pos: Pos::new(1, 1, 0, filename, 0),
        }
    }

    fn advance(&mut self) -> Result<()> {
        match self.bytes_iter.next() {
            Some(Ok(b'\n')) => {
                self.pos.line += 1;
                self.pos.column = 1;
                self.pos.byte += 1;
            }
            Some(Err(error)) => return Err(error.into()),
            None => return Err(Error::Eof),
            _ => {
                self.pos.column += 1;
                self.pos.byte += 1;
            }
        }
        Ok(())
    }

    fn current_pos(&self) -> Pos {
        self.pos
    }

    fn save_start(&mut self) {
        self.saved_pos = self.current_pos();
    }

    fn current_char(&mut self) -> Result<char> {
        if let Some(&Ok(byte)) = self.bytes_iter.peek() {
            return Ok(byte as char);
        }

        match self.bytes_iter.next() {
            Some(Ok(_)) => unreachable!(),
            Some(Err(error)) => Err(error.into()),
            None => Err(Error::Eof),
        }
    }

    fn number(&mut self) -> Result<Token> {
        let buffer = self.take_while(char::is_numeric)?;
        let num = buffer.parse().unwrap();
        self.make_token(Tok::Number(num), num_text_size(num))
    }

    fn simple_token(&mut self, token: Tok) -> Result<Token> {
        let mut pos = self.pos;
        pos.length = 1;
        self.advance()?;
        Ok(Token { pos, token })
    }

    fn make_token(&self, token: Tok, length: usize) -> Result<Token> {
        if length > 10000 {
            panic!();
        }
        let mut pos = self.saved_pos;
        pos.length = length;
        Ok(Token { pos, token })
    }

    fn take_while<F: Fn(char) -> bool>(&mut self, pred: F) -> Result<String> {
        self.save_start();
        let mut buffer = String::new();
        buffer.push(self.current_char()?);
        self.advance()?;
        let mut ch = self.current_char()?;
        while pred(ch) {
            buffer.push(ch);
            self.advance()?;
            ch = self.current_char()?;
        }
        Ok(buffer)
    }

    fn greater_or_greater_equal(&mut self) -> Result<Token> {
        self.two_char_token(vec![('=', Tok::GreaterEqual)], Tok::Greater)
    }

    fn lesser_or_lesser_equal(&mut self) -> Result<Token> {
        self.two_char_token(vec![('=', Tok::LesserEqual)], Tok::Lesser)
    }

    fn bang_or_bang_equal(&mut self) -> Result<Token> {
        self.two_char_token(vec![('=', Tok::BangEqual)], Tok::Bang)
    }

    fn equal_or_equal_equal(&mut self) -> Result<Token> {
        self.two_char_token(vec![('=', Tok::EqualEqual)], Tok::Equal)
    }

    fn identifier(&mut self) -> Result<Token> {
        let ident = self.take_while(|ch| ch.is_alphanumeric() || ch == '_')?;
        let len = ident.len();
        let token = match ident.as_str() {
            "if" => Tok::KeywordIf,
            "else" => Tok::KeywordElse,
            "return" => Tok::KeywordReturn,
            "for" => Tok::KeywordFor,
            "while" => Tok::KeywordWhile,
            _ => Tok::Ident(ident),
        };
        self.make_token(token, len)
    }

    pub fn token(&mut self) -> Result<Token> {
        if let Some(&Ok(ch)) = self.bytes_iter.peek() {
            return match ch {
                b' ' | b'\n' | b'\t' => {
                    self.advance()?;
                    self.token()
                }
                b'a'..=b'z' | b'A'..=b'Z' | b'_' => self.identifier(),
                b'0'..=b'9' => self.number(),
                b'+' => self.simple_token(Tok::Plus),
                b'-' => self.simple_token(Tok::Minus),
                b'*' => self.simple_token(Tok::Star),
                b'/' => self.simple_token(Tok::Slash),
                b'(' => self.simple_token(Tok::LeftParen),
                b')' => self.simple_token(Tok::RightParen),
                b'{' => self.simple_token(Tok::LeftBrace),
                b'}' => self.simple_token(Tok::RightBrace),
                b'>' => self.greater_or_greater_equal(),
                b'<' => self.lesser_or_lesser_equal(),
                b'!' => self.bang_or_bang_equal(),
                b'=' => self.equal_or_equal_equal(),
                b';' => self.simple_token(Tok::Semicolon),
                b'\0' => self.simple_token(Tok::EndOfFile),
                _ => {
                    let mut pos = self.current_pos();
                    pos.length = 1;
                    Err(Error::InvalidToken {
                        pos,
                        start: ch as char,
                    })
                }
            };
        }
        match self.bytes_iter.next() {
            Some(Ok(_)) => unreachable!(),
            Some(Err(error)) => Err(error.into()),
            None => {
                let mut pos = self.pos;
                pos.length = 1;
                Ok(Token {
                    pos,
                    token: Tok::EndOfFile,
                })
            }
        }
    }

    fn two_char_token(&mut self, tokens: Vec<(char, Tok)>, default: Tok) -> Result<Token> {
        self.save_start();
        self.advance()?;
        let token = match self.bytes_iter.peek() {
            Some(&Ok(byte)) => {
                let mut token = None;
                let next_char = byte as char;
                for (ch, tok) in tokens {
                    if ch == next_char {
                        token = Some(tok);
                    }
                }
                token
            }
            _ => None,
        };
        let (token, len) = if let Some(token) = token {
            self.advance()?;
            (token, 2)
        } else {
            (default, 1)
        };
        self.make_token(token, len)
    }
}
