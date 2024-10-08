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

    pub fn token(&mut self) -> Result<Token> {
        if let Some(&Ok(ch)) = self.bytes_iter.peek() {
            return match ch {
                b' ' | b'\n' | b'\t' => {
                    self.advance()?;
                    self.token()
                }
                b'0'..=b'9' => self.number(),
                b'+' => self.simple_token(Tok::Plus),
                b'-' => self.simple_token(Tok::Minus),
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
}
