use std::{io::Read, result};

use crate::{
    ast::{BinaryOperator, Expr, ExprWithPos},
    error::Error::{self, UnexpectedToken},
    lexer::Lexer,
    position::WithPos,
    symbol::Symbols,
    token::{
        Tok::{self, Minus, Number, Plus, Slash, Star, LEFT_PAREN, RIGHT_PAREN},
        Token,
    },
};

macro_rules! eat {
    ($_self:ident, $pat:ident, $var:ident) => {
        match $_self.token() {
            Ok(token) => match token.token {
                $pat(var) => {
                    $var = var;
                    token.pos
                }
                tok => {
                    return Err(UnexpectedToken {
                        expected: stringify!($pat).to_lowercase(),
                        pos: token.pos,
                        unexpected: tok,
                    })
                }
            },
            Err(error) => return Err(error),
        }
    };
    ($_self:ident, $pat:ident) => {
        eat!($_self, $pat, stringify!($pat).to_lowercase())
    };
    ($_self:ident, $pat:ident, $expected:expr) => {
        match $_self.token() {
            Ok(token) => match token.token {
                $pat => token.pos,
                tok => {
                    return Err(UnexpectedToken {
                        expected: $expected,
                        pos: token.pos,
                        unexpected: tok,
                    })
                }
            },
            Err(error) => return Err(error),
        }
    };
}

pub type Result<T> = result::Result<T, Error>;

pub struct Parser<'a, R: Read> {
    lexer: Lexer<R>,
    lookahead: Option<Result<Token>>,
    symbols: &'a mut Symbols<()>,
}

impl<'a, R: Read> Parser<'a, R> {
    pub fn new(lexer: Lexer<R>, symbols: &'a mut Symbols<()>) -> Self {
        Parser {
            lexer,
            lookahead: None,
            symbols,
        }
    }

    // expr = mul ("+" mul | "-" mul)*
    fn expr(&mut self) -> Result<ExprWithPos> {
        let mut expr = self.mul()?;

        loop {
            let op = match self.peek_token() {
                Ok(&Tok::Minus) => WithPos::new(BinaryOperator::Sub, eat!(self, Minus)),
                Ok(&Tok::Plus) => WithPos::new(BinaryOperator::Add, eat!(self, Plus)),
                _ => break,
            };
            let right = Box::new(self.mul()?);
            let pos = expr.pos.grow(right.pos);
            expr = WithPos::new(
                Expr::Binary {
                    left: Box::new(expr),
                    op,
                    right,
                },
                pos,
            );
        }
        Ok(expr)
    }

    // mul = primary ("*" primary | "/" primary)*
    fn mul(&mut self) -> Result<ExprWithPos> {
        let mut expr = self.primary()?;

        loop {
            let op = match self.peek_token() {
                Ok(&Tok::Star) => WithPos::new(BinaryOperator::Mul, eat!(self, Star)),
                Ok(&Tok::Slash) => WithPos::new(BinaryOperator::Div, eat!(self, Slash)),
                _ => break,
            };
            let right = Box::new(self.primary()?);
            let pos = expr.pos.grow(right.pos);
            expr = WithPos::new(
                Expr::Binary {
                    left: Box::new(expr),
                    op,
                    right,
                },
                pos,
            );
        }
        Ok(expr)
    }

    // primary = "(" expr ")" | num
    fn primary(&mut self) -> Result<ExprWithPos> {
        match self.peek()?.token {
            LEFT_PAREN => {
                eat!(self, LEFT_PAREN);
                let expr = self.expr()?;
                eat!(self, RIGHT_PAREN);
                Ok(expr)
            }
            Number(_) => {
                let value;
                let pos = eat!(self, Number, value);
                Ok(WithPos::new(Expr::Number { value }, pos))
            }
            _ => Err(self.unexpected_token("expected `(` or number")?),
        }
    }

    fn peek(&mut self) -> std::result::Result<&Token, &Error> {
        if self.lookahead.is_none() {
            self.lookahead = Some(self.lexer.token());
        }
        // NOTE: lookahead always contain a value, hence unwrap.
        self.lookahead.as_ref().unwrap().as_ref()
    }

    fn peek_token(&mut self) -> std::result::Result<&Tok, &Error> {
        self.peek().map(|token| &token.token)
    }

    fn token(&mut self) -> Result<Token> {
        if let Some(token) = self.lookahead.take() {
            return token;
        }
        self.lexer.token()
    }

    fn unexpected_token(&mut self, expected: &str) -> Result<Error> {
        let token = self.token()?;
        Err(UnexpectedToken {
            expected: expected.to_string(),
            pos: token.pos,
            unexpected: token.token,
        })
    }

    pub fn parse(&mut self) -> Result<ExprWithPos> {
        let expr = self.expr()?;
        match self.token() {
            Ok(Token {
                token: Tok::EndOfFile,
                ..
            })
            | Err(Error::Eof) => Ok(expr),
            _ => Err(self.unexpected_token("end of file")?),
        }
    }
}
