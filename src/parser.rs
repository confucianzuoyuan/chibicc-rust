use std::{cell::RefCell, collections::HashMap, io::Read, rc::Rc, result};

use crate::{
    ast::{
        self, BinaryOperator, Expr, ExprWithPos, Function, Obj, Program, Stmt, StmtWithPos,
        UnaryOperator,
    },
    error::Error::{self, UnexpectedToken},
    lexer::Lexer,
    position::{Pos, WithPos},
    sema::{self, add_type, get_sizeof, sema_stmt, Type, WithType},
    symbol::Symbols,
    token::{
        Tok::{
            self, Amp, BangEqual, Comma, Equal, EqualEqual, Greater, GreaterEqual, Ident,
            KeywordElse, KeywordFor, KeywordIf, KeywordInt, KeywordReturn, KeywordWhile, LeftBrace,
            LeftBracket, LeftParen, Lesser, LesserEqual, Minus, Number, Plus, RightBrace,
            RightBracket, RightParen, Semicolon, Slash, Star,
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
                let mut expr = self.expr()?;
                add_type(&mut expr);
                let node = WithPos::new(Stmt::Return { expr }, pos);
                eat!(self, Semicolon);
                Ok(node)
            }
            Tok::LeftBrace => {
                eat!(self, LeftBrace);
                let stmt = self.compound_stmt()?;
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

    /// declaration = declspec (declarator ("=" expr)? ("," declarator ("=" expr)?)*)? ";"
    fn declaration(&mut self) -> Result<StmtWithPos> {
        let basety = self.declspec()?;

        let mut decls = vec![];

        loop {
            match self.peek()?.token {
                Tok::Comma => {
                    eat!(self, Comma);
                }
                Tok::Semicolon => {
                    eat!(self, Semicolon);
                    break;
                }
                _ => {
                    let ty = self.declarator(basety.clone())?;
                    let ident;
                    match ty.clone() {
                        Type::TyInt {
                            name:
                                Some(Token {
                                    token: Tok::Ident(ident_name),
                                    ..
                                }),
                        } => {
                            ident = ident_name;
                        }
                        Type::TyPtr {
                            name:
                                Some(Token {
                                    token: Tok::Ident(ident_name),
                                    ..
                                }),
                            ..
                        } => {
                            ident = ident_name;
                        }
                        Type::TyArray {
                            name:
                                Some(Token {
                                    token: Tok::Ident(ident_name),
                                    ..
                                }),
                            ..
                        } => {
                            ident = ident_name;
                        }
                        _ => panic!("ident must have a type."),
                    }
                    if self.locals.get(&ident).is_none() {
                        self.locals.insert(
                            ident.clone(),
                            Rc::new(RefCell::new(Obj {
                                name: ident.clone(),
                                offset: 0,
                                ty: ty.clone(),
                            })),
                        );
                    } else {
                        panic!("{} has been already declared.", ident);
                    }

                    match self.peek()?.token {
                        Tok::Equal => {
                            eat!(self, Equal);
                        }
                        _ => continue,
                    }

                    let var = self.locals.get(&ident).unwrap().clone();

                    let lhs = WithPos::new(
                        WithType::new(Expr::Variable { obj: var }, ty.clone()),
                        Pos::dummy(),
                    );
                    let rhs = self.assign()?;
                    let node = WithPos::new(
                        WithType::new(
                            Expr::Assign {
                                l_value: Box::new(lhs),
                                r_value: Box::new(rhs),
                            },
                            ty.clone(),
                        ),
                        Pos::dummy(),
                    );

                    let expr_stmt = WithPos::new(Stmt::ExprStmt { expr: node }, Pos::dummy());
                    decls.push(expr_stmt);
                }
            }
        }

        Ok(WithPos::new(Stmt::Block { body: decls }, Pos::dummy()))
    }

    /// declspec = "int"
    fn declspec(&mut self) -> Result<Type> {
        eat!(self, KeywordInt);
        Ok(Type::TyInt { name: None })
    }

    /// func-params = (param ("," param)*)? ")"
    /// param       = declspec declarator
    fn func_params(&mut self, ty: Type) -> Result<Type> {
        let mut params = vec![];
        loop {
            match self.peek()?.token {
                Tok::Comma => {
                    eat!(self, Comma);
                }
                Tok::RightParen => {
                    eat!(self, RightParen);
                    break;
                }
                _ => {
                    let basety = self.declspec()?;
                    let ty = self.declarator(basety)?;
                    params.push(ty);
                }
            }
        }

        Ok(Type::TyFunc {
            name: None,
            params,
            return_ty: Box::new(ty),
        })
    }

    /// type-suffix = "(" func-params
    ///             | "[" num "]" type-suffix
    ///             | ε
    fn type_suffix(&mut self, mut ty: Type) -> Result<Type> {
        match self.peek()?.token {
            Tok::LeftParen => {
                eat!(self, LeftParen);
                self.func_params(ty)
            }
            Tok::LeftBracket => {
                eat!(self, LeftBracket);
                let num;
                eat!(self, Number, num);
                eat!(self, RightBracket);
                ty = self.type_suffix(ty)?;
                Ok(Type::TyArray {
                    name: None,
                    base: Box::new(ty),
                    array_len: num as i32,
                })
            }
            _ => Ok(ty),
        }
    }

    /// declarator = "*"* ident type-suffix
    fn declarator(&mut self, ty: Type) -> Result<Type> {
        let mut ty = ty.clone();
        loop {
            match self.peek()?.token {
                Tok::Star => {
                    eat!(self, Star);
                    ty = sema::pointer_to(ty);
                }
                _ => break,
            }
        }

        match self.peek()?.token {
            Tok::Ident(..) => {
                let tok = self.token()?;
                ty = self.type_suffix(ty)?;
                match ty {
                    Type::TyInt { ref mut name } => *name = Some(tok.clone()),
                    Type::TyPtr { ref mut name, .. } => *name = Some(tok.clone()),
                    Type::TyFunc { ref mut name, .. } => *name = Some(tok.clone()),
                    Type::TyArray { ref mut name, .. } => *name = Some(tok.clone()),
                    _ => (),
                }
            }
            _ => panic!("expected a variable name."),
        }

        Ok(ty)
    }

    /// compound-stmt = (declaration | stmt)* "}"
    fn compound_stmt(&mut self) -> Result<StmtWithPos> {
        let mut stmts = vec![];
        loop {
            match self.peek()?.token {
                Tok::RightBrace => {
                    eat!(self, RightBrace);
                    break;
                }
                Tok::KeywordInt => {
                    let mut declarations = self.declaration()?;
                    sema_stmt(&mut declarations);
                    stmts.push(declarations);
                }
                _ => {
                    let mut stmt = self.stmt()?;
                    sema_stmt(&mut stmt);
                    stmts.push(stmt);
                }
            }
        }
        Ok(WithPos::new(Stmt::Block { body: stmts }, Pos::dummy()))
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

    /// expr = assign
    fn expr(&mut self) -> Result<ExprWithPos> {
        self.assign()
    }

    /// assign = equality ("=" assign)?
    fn assign(&mut self) -> Result<ExprWithPos> {
        let mut expr = self.equality()?;
        match self.peek()?.token {
            Tok::Equal => {
                let pos = eat!(self, Equal);
                let r_value = self.assign()?;
                expr = WithPos::new(
                    WithType::new(
                        Expr::Assign {
                            l_value: Box::new(expr),
                            r_value: Box::new(r_value),
                        },
                        Type::TyPlaceholder,
                    ),
                    pos,
                )
            }
            _ => (),
        }
        Ok(expr)
    }

    /// equality = relational ("==" relational | "!=" relational)*
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
                WithType::new(
                    Expr::Binary {
                        left: Box::new(expr),
                        op,
                        right,
                    },
                    Type::TyPlaceholder,
                ),
                pos,
            );
        }
        Ok(expr)
    }

    /// relational = add ("<" add | "<=" add | ">" add | ">=" add)*
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
                WithType::new(
                    Expr::Binary {
                        left: Box::new(expr),
                        op,
                        right,
                    },
                    Type::TyPlaceholder,
                ),
                pos,
            );
        }
        Ok(expr)
    }

    /// add = mul ("+" mul | "-" mul)*
    fn add(&mut self) -> Result<ExprWithPos> {
        let mut expr = self.mul()?;

        loop {
            match self.peek()?.token {
                // In C, `+` operator is overloaded to perform the pointer arithmetic.
                // If p is a pointer, p+n adds not n but sizeof(*p)*n to the value of p,
                // so that p+n points to the location n elements (not bytes) ahead of p.
                // In other words, we need to scale an integer value before adding to a
                // pointer value. This function takes care of the scaling.
                Tok::Plus => {
                    let pos = eat!(self, Plus);
                    let mut right = self.mul()?;
                    add_type(&mut expr);
                    add_type(&mut right);

                    match (expr.node.ty.clone(), right.node.ty.clone()) {
                        // num + num
                        (Type::TyInt { .. }, Type::TyInt { .. }) => {
                            expr = WithPos::new(
                                WithType::new(
                                    Expr::Binary {
                                        left: Box::new(expr),
                                        op: WithPos::new(ast::BinaryOperator::Add, pos),
                                        right: Box::new(right),
                                    },
                                    Type::TyInt { name: None },
                                ),
                                pos,
                            );
                        }
                        // ptr + num
                        (
                            Type::TyPtr { base, .. }
                            | Type::TyArray {
                                name: _,
                                base,
                                array_len: _,
                            },
                            Type::TyInt { .. },
                        ) => {
                            // num * 8
                            right = WithPos::new(
                                WithType::new(
                                    Expr::Binary {
                                        left: Box::new(right),
                                        op: WithPos::new(ast::BinaryOperator::Mul, pos),
                                        right: Box::new(WithPos::new(
                                            WithType::new(
                                                Expr::Number {
                                                    value: get_sizeof(*base) as i64,
                                                },
                                                Type::TyInt { name: None },
                                            ),
                                            pos,
                                        )),
                                    },
                                    Type::TyInt { name: None },
                                ),
                                pos,
                            );
                            expr = WithPos::new(
                                WithType::new(
                                    Expr::Binary {
                                        left: Box::new(expr.clone()),
                                        op: WithPos::new(ast::BinaryOperator::Add, pos),
                                        right: Box::new(right),
                                    },
                                    expr.node.ty.clone(),
                                ),
                                pos,
                            );
                        }
                        // num + ptr
                        (
                            Type::TyInt { .. },
                            Type::TyPtr { base, .. } | Type::TyArray { base, .. },
                        ) => {
                            // num * 8
                            expr = WithPos::new(
                                WithType::new(
                                    Expr::Binary {
                                        left: Box::new(expr),
                                        op: WithPos::new(ast::BinaryOperator::Mul, pos),
                                        right: Box::new(WithPos::new(
                                            WithType::new(
                                                Expr::Number {
                                                    value: get_sizeof(*base) as i64,
                                                },
                                                Type::TyInt { name: None },
                                            ),
                                            pos,
                                        )),
                                    },
                                    Type::TyInt { name: None },
                                ),
                                pos,
                            );
                            expr = WithPos::new(
                                WithType::new(
                                    Expr::Binary {
                                        left: Box::new(right.clone()),
                                        op: WithPos::new(ast::BinaryOperator::Add, pos),
                                        right: Box::new(expr),
                                    },
                                    right.node.ty.clone(),
                                ),
                                pos,
                            );
                        }
                        // other
                        _ => panic!("invalid operands for pointer arithmetic add."),
                    }
                }
                // Like `+`, `-` is overloaded for the pointer type.
                Tok::Minus => {
                    let pos = eat!(self, Minus);
                    let mut right = self.mul()?;
                    add_type(&mut expr);
                    add_type(&mut right);

                    match (expr.node.ty.clone(), right.node.ty.clone()) {
                        // num - num
                        (Type::TyInt { .. }, Type::TyInt { .. }) => {
                            expr = WithPos::new(
                                WithType::new(
                                    Expr::Binary {
                                        left: Box::new(expr),
                                        op: WithPos::new(ast::BinaryOperator::Sub, pos),
                                        right: Box::new(right),
                                    },
                                    Type::TyInt { name: None },
                                ),
                                pos,
                            );
                        }
                        // ptr - num
                        (Type::TyPtr { base, .. }, Type::TyInt { .. }) => {
                            // num * 8
                            right = WithPos::new(
                                WithType::new(
                                    Expr::Binary {
                                        left: Box::new(right),
                                        op: WithPos::new(ast::BinaryOperator::Mul, pos),
                                        right: Box::new(WithPos::new(
                                            WithType::new(
                                                Expr::Number {
                                                    value: get_sizeof(*base) as i64,
                                                },
                                                Type::TyInt { name: None },
                                            ),
                                            pos,
                                        )),
                                    },
                                    Type::TyInt { name: None },
                                ),
                                pos,
                            );
                            add_type(&mut right);
                            expr = WithPos::new(
                                WithType::new(
                                    Expr::Binary {
                                        left: Box::new(expr.clone()),
                                        op: WithPos::new(ast::BinaryOperator::Sub, pos),
                                        right: Box::new(right),
                                    },
                                    expr.node.ty.clone(),
                                ),
                                pos,
                            );
                        }
                        // ptr - ptr, which returns how many elements are between the two.
                        (Type::TyPtr { base, .. }, Type::TyPtr { .. }) => {
                            let left = WithPos::new(
                                WithType::new(
                                    Expr::Binary {
                                        left: Box::new(expr),
                                        op: WithPos::new(ast::BinaryOperator::Sub, pos),
                                        right: Box::new(right),
                                    },
                                    Type::TyInt { name: None },
                                ),
                                pos,
                            );
                            expr = WithPos::new(
                                WithType::new(
                                    Expr::Binary {
                                        left: Box::new(left),
                                        op: WithPos::new(ast::BinaryOperator::Div, pos),
                                        right: Box::new(WithPos::new(
                                            WithType::new(
                                                Expr::Number {
                                                    value: get_sizeof(*base) as i64,
                                                },
                                                Type::TyInt { name: None },
                                            ),
                                            pos,
                                        )),
                                    },
                                    Type::TyInt { name: None },
                                ),
                                pos,
                            );
                        }
                        // other
                        _ => panic!("invalid operands for pointer arithmetic sub."),
                    }
                }
                _ => break,
            }
        }
        Ok(expr)
    }

    /// mul = unary ("*" unary | "/" unary)*
    fn mul(&mut self) -> Result<ExprWithPos> {
        let mut expr = self.unary()?;

        loop {
            let op = match self.peek_token() {
                Ok(&Tok::Star) => WithPos::new(BinaryOperator::Mul, eat!(self, Star)),
                Ok(&Tok::Slash) => WithPos::new(BinaryOperator::Div, eat!(self, Slash)),
                _ => break,
            };
            let mut right = Box::new(self.unary()?);
            add_type(&mut expr);
            add_type(&mut right);
            let pos = expr.pos.grow(right.pos);
            expr = WithPos::new(
                WithType::new(
                    Expr::Binary {
                        left: Box::new(expr.clone()),
                        op,
                        right,
                    },
                    expr.node.ty.clone(),
                ),
                pos,
            );
        }
        Ok(expr)
    }

    /// unary = ("+" | "-" | "*" | "&") unary
    ///       | primary
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
                    WithType::new(
                        Expr::Unary {
                            op,
                            expr: Box::new(expr),
                        },
                        Type::TyPlaceholder,
                    ),
                    pos,
                ))
            }
            Amp => {
                let pos = eat!(self, Amp);
                let mut expr = self.unary()?;
                add_type(&mut expr);
                Ok(WithPos::new(
                    WithType::new(
                        Expr::Addr {
                            expr: Box::new(expr),
                        },
                        Type::TyPlaceholder,
                    ),
                    pos,
                ))
            }
            Star => {
                let pos = eat!(self, Star);
                let mut expr = self.unary()?;
                add_type(&mut expr);
                Ok(WithPos::new(
                    WithType::new(
                        Expr::Deref {
                            expr: Box::new(expr),
                        },
                        Type::TyPlaceholder,
                    ),
                    pos,
                ))
            }
            _ => self.primary(),
        }
    }

    /// funcall = ident "(" (assign ("," assign)*)? ")"
    fn funcall(&mut self, funname: String) -> Result<ExprWithPos> {
        let pos = eat!(self, LeftParen);
        let mut args = vec![];

        loop {
            match self.peek()?.token {
                Tok::RightParen => {
                    eat!(self, RightParen);
                    break;
                }
                Tok::Comma => {
                    eat!(self, Comma);
                }
                _ => {
                    let mut arg_exp = self.assign()?;
                    add_type(&mut arg_exp);
                    args.push(arg_exp);
                }
            }
        }

        Ok(WithPos::new(
            WithType::new(
                Expr::FunctionCall {
                    name: funname,
                    args,
                },
                Type::TyInt { name: None },
            ),
            pos,
        ))
    }

    /// primary = "(" expr ")" | ident args? | num
    /// args = "(" ")"
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
                Ok(WithPos::new(
                    WithType::new(Expr::Number { value }, Type::TyInt { name: None }),
                    pos,
                ))
            }
            Tok::Ident(_) => {
                let name;
                let pos = eat!(self, Ident, name);
                // function call
                if self.peek()?.token == Tok::LeftParen {
                    return self.funcall(name.clone());
                }
                // variable
                if self.locals.get(&name).is_none() {
                    self.locals.insert(
                        name.clone(),
                        Rc::new(RefCell::new(Obj {
                            name: name.clone(),
                            offset: 0,
                            ty: Type::TyPlaceholder,
                        })),
                    );
                }
                let var = self.locals.get(&name).unwrap().clone();
                let _ty = var.borrow().ty.clone();
                Ok(WithPos::new(
                    WithType::new(Expr::Variable { obj: var }, _ty),
                    pos,
                ))
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

    fn function(&mut self) -> Result<Function> {
        let mut ty = self.declspec()?;
        ty = self.declarator(ty)?;

        self.locals = HashMap::new();

        let ident;
        match ty.clone() {
            Type::TyFunc {
                name:
                    Some(Token {
                        token: Tok::Ident(ident_name),
                        ..
                    }),
                params,
                ..
            } => {
                ident = ident_name;
                for p in params {
                    match p.clone() {
                        Type::TyInt {
                            name:
                                Some(Token {
                                    pos: _,
                                    token: Tok::Ident(param_name),
                                }),
                        } => {
                            self.locals.insert(
                                param_name.clone(),
                                Rc::new(RefCell::new(Obj {
                                    name: param_name,
                                    offset: 0,
                                    ty: p.clone(),
                                })),
                            );
                        }
                        _ => panic!(),
                    }
                }
            }
            _ => panic!("ident must have a type."),
        }

        eat!(self, LeftBrace);
        let body = self.compound_stmt()?;
        Ok(ast::Function {
            name: ident,
            params: self.locals.clone(),
            body,
            locals: self.locals.clone(),
            stack_size: 0,
        })
    }

    /// program = function-definition*
    pub fn parse(&mut self) -> Result<Program> {
        let mut funcs = vec![];
        loop {
            match self.peek() {
                Ok(Token {
                    token: Tok::EndOfFile,
                    ..
                })
                | Err(Error::Eof) => break,
                _ => funcs.push(self.function()?),
            }
        }
        Ok(funcs)
    }
}
