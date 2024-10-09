use std::{cell::RefCell, collections::HashMap, io::Read, rc::Rc, result};

use crate::{
    ast::{
        self, BinaryOperator, Expr, ExprWithPos, Obj, Program, Stmt, StmtWithPos, UnaryOperator,
    },
    error::Error::{self, UnexpectedToken},
    lexer::Lexer,
    position::WithPos,
    symbol::Symbols,
    token::{
        Tok::{
            self, BangEqual, Equal, EqualEqual, Greater, GreaterEqual, Ident, KeywordElse,
            KeywordFor, KeywordIf, KeywordReturn, KeywordWhile, LeftBrace, LeftParen, Lesser,
            LesserEqual, Minus, Number, Plus, RightBrace, RightParen, Semicolon, Slash, Star,
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
    /// All local variable instances created during parsing are
    /// accumulated to this map
    locals: HashMap<String, Rc<RefCell<Obj>>>,
}

impl<'a, R: Read> Parser<'a, R> {
    pub fn new(lexer: Lexer<R>, symbols: &'a mut Symbols<()>) -> Self {
        Parser {
            lexer,
            lookahead: None,
            symbols,
            locals: HashMap::new(),
        }
    }

    /// stmt = "return" expr ";"
    ///      | "if" "(" expr ")" stmt ("else" stmt)?
    ///      | "for" "(" expr-stmt expr? ";" expr? ")" stmt
    ///      | "while" "(" expr ")"
    ///      | "{" compound-stmt
    ///      | expr-stmt
    fn stmt(&mut self) -> Result<StmtWithPos> {
        match self.peek()?.token {
            Tok::KeywordReturn => {
                let pos = eat!(self, KeywordReturn);
                let expr = self.expr()?;
                let node = WithPos::new(Stmt::Return { expr }, pos);
                eat!(self, Semicolon);
                Ok(node)
            }
            Tok::LeftBrace => {
                let stmt = self.compound_stmt()?;
                eat!(self, RightBrace);
                Ok(stmt)
            }
            Tok::KeywordIf => {
                let pos = eat!(self, KeywordIf);
                eat!(self, LeftParen);
                let condition = self.expr()?;
                eat!(self, RightParen);
                let then_clause = self.stmt()?;
                let else_clause = match self.peek()?.token {
                    Tok::KeywordElse => {
                        eat!(self, KeywordElse);
                        Some(self.stmt()?)
                    }
                    _ => None,
                };
                Ok(WithPos::new(
                    Stmt::IfStmt {
                        condition,
                        then_clause: Box::new(then_clause),
                        else_clause: if let Some(els) = else_clause {
                            Some(Box::new(els))
                        } else {
                            None
                        },
                    },
                    pos,
                ))
            }
            Tok::KeywordFor => {
                let pos = eat!(self, KeywordFor);
                eat!(self, LeftParen);

                let init = self.expr_stmt()?;

                let cond = match self.peek()?.token {
                    Tok::Semicolon => None,
                    _ => Some(self.expr()?),
                };

                eat!(self, Semicolon);

                let inc = match self.peek()?.token {
                    Tok::RightParen => None,
                    _ => Some(self.expr()?),
                };

                eat!(self, RightParen);

                let body = self.stmt()?;

                Ok(WithPos::new(
                    Stmt::ForStmt {
                        init: Box::new(init),
                        condition: cond,
                        body: Box::new(body),
                        increment: inc,
                    },
                    pos,
                ))
            }
            Tok::KeywordWhile => {
                let pos = eat!(self, KeywordWhile);
                eat!(self, LeftParen);
                let cond = self.expr()?;
                eat!(self, RightParen);
                let body = self.stmt()?;
                Ok(WithPos::new(
                    Stmt::WhileStmt {
                        condition: cond,
                        body: Box::new(body),
                    },
                    pos,
                ))
            }
            _ => self.expr_stmt(),
        }
    }

    /// compound-stmt = stmt* "}"
    fn compound_stmt(&mut self) -> Result<StmtWithPos> {
        let pos = eat!(self, LeftBrace);
        let mut stmts = vec![];
        loop {
            match self.peek()?.token {
                Tok::RightBrace => break,
                _ => stmts.push(self.stmt()?),
            }
        }
        Ok(WithPos::new(Stmt::Block { body: stmts }, pos))
    }

    /// expr-stmt = expr? ";"
    fn expr_stmt(&mut self) -> Result<StmtWithPos> {
        match self.peek()?.token {
            Tok::Semicolon => {
                let pos = eat!(self, Semicolon);
                Ok(WithPos::new(Stmt::NullStmt, pos))
            }
            _ => {
                let expr = self.expr()?;
                let pos = eat!(self, Semicolon);
                Ok(WithPos::new(Stmt::ExprStmt { expr }, pos))
            }
        }
    }

    // expr = assign
    fn expr(&mut self) -> Result<ExprWithPos> {
        self.assign()
    }

    // assign = equality ("=" assign)?
    fn assign(&mut self) -> Result<ExprWithPos> {
        let mut expr = self.equality()?;
        match self.peek()?.token {
            Tok::Equal => {
                let pos = eat!(self, Equal);
                let r_value = self.assign()?;
                expr = WithPos::new(
                    Expr::Assign {
                        l_value: Box::new(expr),
                        r_value: Box::new(r_value),
                    },
                    pos,
                )
            }
            _ => (),
        }
        Ok(expr)
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

    // primary = "(" expr ")" | ident | num
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
            Tok::Ident(_) => {
                let name;
                let pos = eat!(self, Ident, name);
                if self.locals.get(&name).is_none() {
                    self.locals.insert(
                        name.clone(),
                        Rc::new(RefCell::new(Obj {
                            name: name.clone(),
                            offset: 0,
                        })),
                    );
                }
                let var = self.locals.get(&name).unwrap().clone();
                Ok(WithPos::new(Expr::Variable(var), pos))
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
        let mut stmts = vec![];
        loop {
            match self.peek() {
                Ok(Token {
                    token: Tok::EndOfFile,
                    ..
                })
                | Err(Error::Eof) => break,
                _ => stmts.push(self.stmt()?),
            }
        }
        Ok(ast::Function {
            body: stmts,
            locals: self.locals.clone(),
            stack_size: 0,
        })
    }
}
