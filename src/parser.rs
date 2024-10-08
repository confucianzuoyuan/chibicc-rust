use std::{io::Read, result};

use crate::{
    ast::{BinaryOperator, Expr, ExprWithPos, Program, Stmt, StmtWithPos, UnaryOperator},
    error::Error::{self, UnexpectedToken},
    lexer::Lexer,
    position::WithPos,
    symbol::Symbols,
    token::{
        Tok::{
            self, BangEqual, EqualEqual, Greater, GreaterEqual, LeftParen, Lesser, LesserEqual,
            Minus, Number, Plus, RightParen, Semicolon, Slash, Star,
        },
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

    // stmt = expr-stmt
    fn stmt(&mut self) -> Result<StmtWithPos> {
        self.expr_stmt()
    }

    // expr-stmt = expr ";"
    fn expr_stmt(&mut self) -> Result<StmtWithPos> {
        let expr = self.expr()?;
        let pos = eat!(self, Semicolon);
        Ok(WithPos::new(Stmt::ExprStmt { expr }, pos))
    }

    // expr = equality
    fn expr(&mut self) -> Result<ExprWithPos> {
        self.equality()
    }

    // equality = relational ("==" relational | "!=" relational)*
    fn equality(&mut self) -> Result<ExprWithPos> {
        let mut expr = self.relational()?;

        loop {
            let op = match self.peek_token() {
                Ok(&Tok::EqualEqual) => WithPos::new(BinaryOperator::Eq, eat!(self, EqualEqual)),
                Ok(&Tok::BangEqual) => WithPos::new(BinaryOperator::Ne, eat!(self, BangEqual)),
                _ => break,
            };
            let right = Box::new(self.relational()?);
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

    // relational = add ("<" add | "<=" add | ">" add | ">=" add)*
    fn relational(&mut self) -> Result<ExprWithPos> {
        let mut expr = self.add()?;

        loop {
            let op = match self.peek_token() {
                Ok(&Tok::Lesser) => WithPos::new(BinaryOperator::Lt, eat!(self, Lesser)),
                Ok(&Tok::LesserEqual) => WithPos::new(BinaryOperator::Le, eat!(self, LesserEqual)),
                Ok(&Tok::Greater) => WithPos::new(BinaryOperator::Gt, eat!(self, Greater)),
                Ok(&Tok::GreaterEqual) => {
                    WithPos::new(BinaryOperator::Ge, eat!(self, GreaterEqual))
                }
                _ => break,
            };
            let right = Box::new(self.add()?);
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

    // add = mul ("+" mul | "-" mul)*
    fn add(&mut self) -> Result<ExprWithPos> {
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

    // mul = unary ("*" unary | "/" unary)*
    fn mul(&mut self) -> Result<ExprWithPos> {
        let mut expr = self.unary()?;

        loop {
            let op = match self.peek_token() {
                Ok(&Tok::Star) => WithPos::new(BinaryOperator::Mul, eat!(self, Star)),
                Ok(&Tok::Slash) => WithPos::new(BinaryOperator::Div, eat!(self, Slash)),
                _ => break,
            };
            let right = Box::new(self.unary()?);
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

    // unary = ("+" | "-") unary
    //       | primary
    fn unary(&mut self) -> Result<ExprWithPos> {
        match self.peek()?.token {
            Plus => {
                eat!(self, Plus);
                Ok(self.unary()?)
            }
            Minus => {
                let op = WithPos::new(UnaryOperator::Neg, eat!(self, Minus));
                let expr = self.unary()?;
                let pos = expr.pos.grow(expr.pos);
                Ok(WithPos::new(
                    Expr::Unary {
                        op,
                        expr: Box::new(expr),
                    },
                    pos,
                ))
            }
            _ => self.primary(),
        }
    }

    // primary = "(" expr ")" | num
    fn primary(&mut self) -> Result<ExprWithPos> {
        match self.peek()?.token {
            LeftParen => {
                eat!(self, LeftParen);
                let expr = self.expr()?;
                eat!(self, RightParen);
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

    // program = stmt*
    pub fn parse(&mut self) -> Result<Program> {
        let mut program = vec![];
        loop {
            match self.peek() {
                Ok(Token {
                    token: Tok::EndOfFile,
                    ..
                })
                | Err(Error::Eof) => break,
                _ => program.push(self.stmt()?),
            }
        }
        Ok(program)
    }
}
