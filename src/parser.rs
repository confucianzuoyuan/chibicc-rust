use std::{cell::RefCell, rc::Rc, result};

use crate::{
    ast::{
        self, BinaryOperator, Expr, ExprWithPos, Function, InitData, Obj, Program, Stmt,
        StmtWithPos, UnaryOperator,
    },
    error::Error::{self, UnexpectedToken},
    position::{Pos, WithPos},
    sema::{self, add_type, align_to, get_sizeof, sema_stmt, Type, WithType},
    symbol::{Strings, Symbols},
    token::{
        Tok::{
            self, Amp, BangEqual, Comma, Dot, Equal, EqualEqual, Greater, GreaterEqual, Ident,
            KeywordChar, KeywordElse, KeywordFor, KeywordIf, KeywordInt, KeywordLong,
            KeywordReturn, KeywordShort, KeywordSizeof, KeywordStruct, KeywordUnion, KeywordWhile,
            LeftBrace, LeftBracket, LeftParen, Lesser, LesserEqual, Minus, MinusGreater, Number,
            Plus, RightBrace, RightBracket, RightParen, Semicolon, Slash, Star, Str,
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

pub struct Parser<'a> {
    tokens: Vec<Token>,
    current_pos: usize,
    symbols: &'a mut Symbols<()>,
    /// All local variable instances created during parsing are
    /// accumulated to this map
    locals: Vec<Rc<RefCell<Obj>>>,
    globals: Vec<Rc<RefCell<Obj>>>,
    functions: Vec<Function>,
    unique_name_count: i32,

    var_env: Symbols<Rc<RefCell<Obj>>>,
    struct_tag_env: Symbols<Type>,
}

impl<'a> Parser<'a> {
    pub fn new(tokens: Vec<Token>, symbols: &'a mut Symbols<()>, strings: Rc<Strings>) -> Self {
        let var_env = Symbols::new(Rc::clone(&strings));
        let struct_tag_env = Symbols::new(Rc::clone(&strings));
        Parser {
            tokens,
            current_pos: 0,
            symbols,
            locals: Vec::new(),
            globals: Vec::new(),
            functions: Vec::new(),
            unique_name_count: 0,

            var_env,
            struct_tag_env,
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
                    let ident = self.get_ident(ty.clone())?;
                    let var = self.new_local_variable(ident, ty.clone())?;

                    match self.peek()?.token {
                        Tok::Equal => {
                            eat!(self, Equal);
                        }
                        _ => continue,
                    }

                    let lhs = WithPos::new(
                        WithType::new(Expr::Variable { obj: var }, ty.clone()),
                        self.peek()?.pos,
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
                        self.peek()?.pos,
                    );

                    let expr_stmt = WithPos::new(Stmt::ExprStmt { expr: node }, self.peek()?.pos);
                    decls.push(expr_stmt);
                }
            }
        }

        Ok(WithPos::new(Stmt::Block { body: decls }, self.peek()?.pos))
    }

    /// struct-decl = "{" struct-members
    /// struct-members = (declspec declarator (","  declarator)* ";")*
    fn struct_decl(&mut self) -> Result<Type> {
        // Read a struct tag.
        let tag = match self.peek()?.token {
            Tok::Ident(..) => {
                let ident;
                eat!(self, Ident, ident);
                Some(ident)
            }
            _ => None,
        };

        match self.peek()?.token {
            Tok::LeftBrace => {
                eat!(self, LeftBrace);
            }
            _ => {
                if let Some(struct_tag) = tag {
                    let ty = self.struct_tag_env.look(self.symbols.symbol(&struct_tag));
                    if let Some(_ty) = ty {
                        return Ok(_ty.clone());
                    } else {
                        panic!("unknown struct type: {}", struct_tag);
                    }
                }
            }
        }

        // Construct a struct object.
        let mut members = vec![];
        loop {
            match self.peek()?.token {
                Tok::RightBrace => {
                    eat!(self, RightBrace);
                    break;
                }
                _ => {
                    let basety = self.declspec()?;
                    loop {
                        match self.peek()?.token {
                            Tok::Semicolon => {
                                eat!(self, Semicolon);
                                break;
                            }
                            Tok::Comma => {
                                eat!(self, Comma);
                            }
                            _ => {
                                let member_ty = self.declarator(basety.clone())?;
                                let member_name = self.get_ident_token(member_ty.clone())?;
                                let member = Rc::new(RefCell::new(sema::Member {
                                    ty: member_ty,
                                    name: member_name,
                                    offset: 0,
                                }));
                                members.push(member);
                            }
                        }
                    }
                }
            }
        }

        let mut struct_align = 1;
        let mut offset = 0;
        for mem in members.clone() {
            offset = align_to(offset, sema::get_align(mem.borrow().ty.clone()));
            mem.borrow_mut().offset = offset;
            offset += get_sizeof(mem.borrow().ty.clone());

            if struct_align < sema::get_align(mem.borrow().ty.clone()) {
                struct_align = sema::get_align(mem.borrow().ty.clone());
            }
        }

        let struct_ty = Type::TyStruct {
            name: None,
            members,
            type_size: sema::align_to(offset, struct_align),
            align: struct_align,
        };
        if let Some(struct_tag) = tag {
            self.struct_tag_env
                .enter(self.symbols.symbol(&struct_tag), struct_ty.clone());
        }
        Ok(struct_ty)
    }

    /// union-decl = "{" struct-members
    /// union-members = (declspec declarator (","  declarator)* ";")*
    fn union_decl(&mut self) -> Result<Type> {
        // Read a union tag.
        let tag = match self.peek()?.token {
            Tok::Ident(..) => {
                let ident;
                eat!(self, Ident, ident);
                Some(ident)
            }
            _ => None,
        };

        match self.peek()?.token {
            Tok::LeftBrace => {
                eat!(self, LeftBrace);
            }
            _ => {
                if let Some(union_tag) = tag {
                    let ty = self.struct_tag_env.look(self.symbols.symbol(&union_tag));
                    if let Some(_ty) = ty {
                        return Ok(_ty.clone());
                    } else {
                        panic!("unknown union type: {}", union_tag);
                    }
                }
            }
        }

        // Construct a union object.
        let mut members = vec![];
        loop {
            match self.peek()?.token {
                Tok::RightBrace => {
                    eat!(self, RightBrace);
                    break;
                }
                _ => {
                    let basety = self.declspec()?;
                    loop {
                        match self.peek()?.token {
                            Tok::Semicolon => {
                                eat!(self, Semicolon);
                                break;
                            }
                            Tok::Comma => {
                                eat!(self, Comma);
                            }
                            _ => {
                                let member_ty = self.declarator(basety.clone())?;
                                let member_name = self.get_ident_token(member_ty.clone())?;
                                let member = Rc::new(RefCell::new(sema::Member {
                                    ty: member_ty,
                                    name: member_name,
                                    offset: 0,
                                }));
                                members.push(member);
                            }
                        }
                    }
                }
            }
        }

        // If union, we don't have to assign offsets because they
        // are already initialized to zero. We need to compute the
        // alignment and the size though.
        let mut union_align = 1;
        let mut type_size = 0;
        for mem in members.clone() {
            if union_align < sema::get_align(mem.borrow().ty.clone()) {
                union_align = sema::get_align(mem.borrow().ty.clone());
            }
            if type_size < sema::get_sizeof(mem.borrow().ty.clone()) {
                type_size = sema::get_sizeof(mem.borrow().ty.clone());
            }
        }

        let union_ty = Type::TyUnion {
            name: None,
            members,
            type_size: sema::align_to(type_size, union_align),
            align: union_align,
        };
        if let Some(struct_tag) = tag {
            self.struct_tag_env
                .enter(self.symbols.symbol(&struct_tag), union_ty.clone());
        }
        Ok(union_ty)
    }

    /// declspec = "int" | "char" | struct-decl
    fn declspec(&mut self) -> Result<Type> {
        match self.peek()?.token {
            Tok::KeywordChar => {
                eat!(self, KeywordChar);
                Ok(Type::TyChar { name: None })
            }
            Tok::KeywordInt => {
                eat!(self, KeywordInt);
                Ok(Type::TyInt { name: None })
            }
            Tok::KeywordShort => {
                eat!(self, KeywordShort);
                Ok(Type::TyShort { name: None })
            }
            Tok::KeywordLong => {
                eat!(self, KeywordLong);
                Ok(Type::TyLong { name: None })
            }
            Tok::KeywordStruct => {
                eat!(self, KeywordStruct);
                self.struct_decl()
            }
            Tok::KeywordUnion => {
                eat!(self, KeywordUnion);
                self.union_decl()
            }
            _ => panic!("unknown type name."),
        }
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
                    Type::TyInt { ref mut name }
                    | Type::TyChar { ref mut name }
                    | Type::TyShort { ref mut name }
                    | Type::TyLong { ref mut name }
                    | Type::TyPtr { ref mut name, .. }
                    | Type::TyFunc { ref mut name, .. }
                    | Type::TyArray { ref mut name, .. }
                    | Type::TyStruct { ref mut name, .. }
                    | Type::TyUnion { ref mut name, .. } => *name = Some(tok.clone()),
                    _ => (),
                }
            }
            _ => panic!("expected a variable name, but got {:?}", self.peek()?),
        }

        Ok(ty)
    }

    /// compound-stmt = (declaration | stmt)* "}"
    fn compound_stmt(&mut self) -> Result<StmtWithPos> {
        let mut stmts = vec![];

        self.var_env.begin_scope();
        self.struct_tag_env.begin_scope();
        loop {
            match self.peek()?.token {
                Tok::RightBrace => {
                    eat!(self, RightBrace);
                    break;
                }
                Tok::KeywordLong
                | Tok::KeywordInt
                | Tok::KeywordChar
                | Tok::KeywordShort
                | Tok::KeywordStruct
                | Tok::KeywordUnion => {
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
        self.struct_tag_env.end_scope();
        self.var_env.end_scope();
        Ok(WithPos::new(Stmt::Block { body: stmts }, self.peek()?.pos))
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

    /// expr = assign ("," expr)?
    fn expr(&mut self) -> Result<ExprWithPos> {
        let node = self.assign()?;
        match self.peek()?.token {
            Tok::Comma => {
                let pos = eat!(self, Comma);
                Ok(WithPos::new(
                    WithType::new(
                        Expr::CommaExpr {
                            left: Box::new(node),
                            right: Box::new(self.expr()?),
                        },
                        Type::TyPlaceholder,
                    ),
                    pos,
                ))
            }
            _ => Ok(node),
        }
    }

    /// assign = equality ("=" assign)?
    fn assign(&mut self) -> Result<ExprWithPos> {
        let mut expr = self.equality()?;
        add_type(&mut expr);
        match self.peek()?.token {
            Tok::Equal => {
                let pos = eat!(self, Equal);
                let r_value = self.assign()?;
                expr = WithPos::new(
                    WithType::new(
                        Expr::Assign {
                            l_value: Box::new(expr.clone()),
                            r_value: Box::new(r_value),
                        },
                        expr.node.ty,
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
                    let right = self.mul()?;
                    expr = self.new_add(expr, right, pos)?;
                }
                // Like `+`, `-` is overloaded for the pointer type.
                Tok::Minus => {
                    let pos = eat!(self, Minus);
                    let mut right = self.mul()?;
                    add_type(&mut expr);
                    add_type(&mut right);

                    match (expr.node.ty.clone(), right.node.ty.clone()) {
                        // num - num
                        (
                            Type::TyInt { .. }
                            | Type::TyChar { .. }
                            | Type::TyShort { .. }
                            | Type::TyLong { .. },
                            Type::TyInt { .. }
                            | Type::TyChar { .. }
                            | Type::TyShort { .. }
                            | Type::TyLong { .. },
                        ) => {
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
            _ => self.postfix(),
        }
    }

    fn new_add(
        &mut self,
        mut lhs: ExprWithPos,
        mut rhs: ExprWithPos,
        pos: Pos,
    ) -> Result<ExprWithPos> {
        add_type(&mut lhs);
        add_type(&mut rhs);

        match (lhs.node.ty.clone(), rhs.node.ty.clone()) {
            // num + num
            (Type::TyInt { .. }, Type::TyInt { .. }) => {
                lhs = WithPos::new(
                    WithType::new(
                        Expr::Binary {
                            left: Box::new(lhs),
                            op: WithPos::new(ast::BinaryOperator::Add, pos),
                            right: Box::new(rhs),
                        },
                        Type::TyInt { name: None },
                    ),
                    pos,
                );
            }
            // ptr + num
            (Type::TyPtr { base, .. } | Type::TyArray { base, .. }, Type::TyInt { .. }) => {
                // num * 8
                rhs = WithPos::new(
                    WithType::new(
                        Expr::Binary {
                            left: Box::new(rhs),
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
                lhs = WithPos::new(
                    WithType::new(
                        Expr::Binary {
                            left: Box::new(lhs.clone()),
                            op: WithPos::new(ast::BinaryOperator::Add, pos),
                            right: Box::new(rhs),
                        },
                        lhs.node.ty.clone(),
                    ),
                    pos,
                );
            }
            // num + ptr
            (Type::TyInt { .. }, Type::TyPtr { base, .. } | Type::TyArray { base, .. }) => {
                // num * 8
                lhs = WithPos::new(
                    WithType::new(
                        Expr::Binary {
                            left: Box::new(lhs),
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
                lhs = WithPos::new(
                    WithType::new(
                        Expr::Binary {
                            left: Box::new(rhs.clone()),
                            op: WithPos::new(ast::BinaryOperator::Add, pos),
                            right: Box::new(lhs),
                        },
                        rhs.node.ty.clone(),
                    ),
                    pos,
                );
            }
            // other
            _ => panic!("invalid operands for pointer arithmetic add."),
        }

        Ok(lhs)
    }

    fn struct_ref(&mut self, node: ExprWithPos) -> Result<ExprWithPos> {
        match node.node.ty.clone() {
            Type::TyStruct { members, .. } | Type::TyUnion { members, .. } => {
                let mem_ident;
                let pos = eat!(self, Ident, mem_ident);
                let mut mem_found = None;
                for mem in members {
                    match mem.borrow().name.token.clone() {
                        Tok::Ident(tok_name) => {
                            if mem_ident == tok_name {
                                mem_found = Some(mem.clone());
                                break;
                            }
                        }
                        _ => (),
                    }
                }

                Ok(WithPos::new(
                    WithType::new(
                        Expr::MemberExpr {
                            strct: Box::new(node),
                            member: match mem_found.clone() {
                                Some(m) => m.clone(),
                                None => panic!(),
                            },
                        },
                        match mem_found.clone() {
                            Some(m) => m.borrow().ty.clone(),
                            None => panic!(),
                        },
                    ),
                    pos,
                ))
            }
            _ => panic!("must be struct or union type."),
        }
    }

    /// postfix = primary ("[" expr "]" | "." ident | "->" ident)*
    fn postfix(&mut self) -> Result<ExprWithPos> {
        let mut node = self.primary()?;

        loop {
            match self.peek()?.token {
                Tok::LeftBracket => {
                    // x[y] is short for *(x+y)
                    let pos = eat!(self, LeftBracket);
                    let idx = self.expr()?;
                    eat!(self, RightBracket);
                    node = WithPos::new(
                        WithType::new(
                            Expr::Deref {
                                expr: Box::new(self.new_add(node, idx, pos)?),
                            },
                            Type::TyPlaceholder,
                        ),
                        pos,
                    )
                }
                Tok::Dot => {
                    eat!(self, Dot);
                    add_type(&mut node);
                    node = self.struct_ref(node)?;
                }
                Tok::MinusGreater => {
                    // x->y is short for (*x).y
                    let pos = eat!(self, MinusGreater);
                    node = WithPos::new(
                        WithType::new(
                            Expr::Deref {
                                expr: Box::new(node),
                            },
                            Type::TyPlaceholder,
                        ),
                        pos,
                    );
                    add_type(&mut node);
                    node = self.struct_ref(node)?;
                }
                _ => break,
            }
        }

        Ok(node)
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

    /// primary = "(" "{" stmt+ "}" ")"
    ///         | "(" expr ")"
    ///         | "sizeof" unary
    ///         | ident func-args?
    ///         | str
    ///         | num
    fn primary(&mut self) -> Result<ExprWithPos> {
        match self.peek()?.token {
            LeftParen => {
                eat!(self, LeftParen);
                match self.peek()?.token {
                    // This is a GNU statement expression.
                    Tok::LeftBrace => {
                        let pos = eat!(self, LeftBrace);
                        let stmt = self.compound_stmt()?.node;
                        let node = match stmt {
                            Stmt::Block { body } => Expr::StmtExpr { body: body.clone() },
                            _ => panic!(),
                        };
                        eat!(self, RightParen);
                        Ok(WithPos::new(WithType::new(node, Type::TyPlaceholder), pos))
                    }
                    _ => {
                        let expr = self.expr()?;
                        eat!(self, RightParen);
                        Ok(expr)
                    }
                }
            }
            Number(_) => {
                let value;
                let pos = eat!(self, Number, value);
                Ok(WithPos::new(
                    WithType::new(Expr::Number { value }, Type::TyInt { name: None }),
                    pos,
                ))
            }
            Tok::KeywordSizeof => {
                let pos = eat!(self, KeywordSizeof);
                let mut node = self.unary()?;
                add_type(&mut node);
                Ok(WithPos::new(
                    WithType::new(
                        Expr::Number {
                            value: get_sizeof(node.node.ty.clone()) as i64,
                        },
                        Type::TyInt { name: None },
                    ),
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
                if let Some(var) = self.var_env.look(self.symbols.symbol(&name)) {
                    let _ty = var.borrow().ty.clone();
                    Ok(WithPos::new(
                        WithType::new(Expr::Variable { obj: var.clone() }, _ty),
                        pos,
                    ))
                } else {
                    panic!("variable undefined: {}", name);
                }
            }
            Tok::Str(..) => {
                // 字符串是全局的数据，所以需要给一个标签，这个标签是字符串的全局变量名称
                // 字符串是字符数组类型，需要添加一个`'\0'`作为结束符。
                let mut string;
                let pos = eat!(self, Str, string);
                let unique_name = format!(".L..{}", self.unique_name_count);
                self.unique_name_count += 1;
                string.push_str("\0");
                let var_type = Type::TyArray {
                    name: None,
                    base: Box::new(Type::TyChar { name: None }),
                    array_len: string.len() as i32, // `+1` means add `'\0'`
                };
                let var = self.new_global_variable(
                    unique_name,
                    var_type.clone(),
                    Some(InitData::StringInitData(string)),
                )?;
                Ok(WithPos::new(
                    WithType::new(Expr::Variable { obj: var.clone() }, var_type),
                    pos,
                ))
            }
            _ => Err(self.unexpected_token("expected `(` or number")?),
        }
    }

    fn peek(&mut self) -> std::result::Result<&Token, &Error> {
        if let Some(tok) = self.tokens.get(self.current_pos) {
            Ok(tok)
        } else {
            panic!()
        }
    }

    fn peek_token(&mut self) -> std::result::Result<&Tok, &Error> {
        self.peek().map(|token| &token.token)
    }

    fn token(&mut self) -> Result<Token> {
        if let Some(tok) = self.tokens.get(self.current_pos) {
            self.current_pos += 1;
            Ok(tok.clone())
        } else {
            panic!()
        }
    }

    fn unexpected_token(&mut self, expected: &str) -> Result<Error> {
        let token = self.token()?;
        Err(UnexpectedToken {
            expected: expected.to_string(),
            pos: token.pos,
            unexpected: token.token,
        })
    }

    fn function(&mut self, pos: usize, basety: Type) -> Result<usize> {
        self.current_pos = pos;

        let ty = self.declarator(basety)?;

        match ty.clone() {
            Type::TyFunc { ref mut name, .. } => *name = Some(self.get_ident_token(ty.clone())?),
            _ => panic!("{:?} must be function name.", self.get_ident(ty)?),
        }

        self.locals = Vec::new();

        self.var_env.begin_scope();
        self.struct_tag_env.begin_scope();

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
                    let param_name = self.get_ident(p.clone())?;
                    self.new_local_variable(param_name, p)?;
                }
            }
            _ => panic!("ident must have a type."),
        }

        // 先将函数的参数拷贝一份，因为后面在解析函数体时会加入新的局部变量
        let params = self.locals.clone();

        eat!(self, LeftBrace);
        let body = self.compound_stmt()?;

        self.struct_tag_env.end_scope();
        self.var_env.end_scope();

        self.functions.push(ast::Function {
            name: ident,
            params,
            body,
            locals: self.locals.clone(),
            stack_size: 0,
        });
        Ok(self.current_pos)
    }

    fn is_function(&mut self) -> Result<bool> {
        match self.peek()?.token {
            Tok::Semicolon => {
                eat!(self, Semicolon);
                Ok(false)
            }
            _ => {
                let dummy = Type::TyPlaceholder;
                let ty = self.declarator(dummy)?;
                match ty {
                    Type::TyFunc { .. } => Ok(true),
                    _ => Ok(false),
                }
            }
        }
    }

    fn get_ident_token(&mut self, ty: Type) -> Result<Token> {
        match ty {
            Type::TyArray { name: Some(t), .. }
            | Type::TyInt { name: Some(t), .. }
            | Type::TyChar { name: Some(t), .. }
            | Type::TyShort { name: Some(t), .. }
            | Type::TyLong { name: Some(t), .. }
            | Type::TyPtr { name: Some(t), .. }
            | Type::TyStruct { name: Some(t), .. }
            | Type::TyUnion { name: Some(t), .. }
            | Type::TyFunc { name: Some(t), .. } => Ok(t),
            _ => panic!("ty {:?} has no token.", ty),
        }
    }

    fn get_ident(&mut self, ty: Type) -> Result<String> {
        match ty {
            Type::TyArray {
                name:
                    Some(Token {
                        token: Tok::Ident(param_name),
                        ..
                    }),
                ..
            }
            | Type::TyInt {
                name:
                    Some(Token {
                        token: Tok::Ident(param_name),
                        ..
                    }),
                ..
            }
            | Type::TyChar {
                name:
                    Some(Token {
                        token: Tok::Ident(param_name),
                        ..
                    }),
                ..
            }
            | Type::TyShort {
                name:
                    Some(Token {
                        token: Tok::Ident(param_name),
                        ..
                    }),
                ..
            }
            | Type::TyLong {
                name:
                    Some(Token {
                        token: Tok::Ident(param_name),
                        ..
                    }),
                ..
            }
            | Type::TyPtr {
                name:
                    Some(Token {
                        token: Tok::Ident(param_name),
                        ..
                    }),
                ..
            }
            | Type::TyFunc {
                name:
                    Some(Token {
                        token: Tok::Ident(param_name),
                        ..
                    }),
                ..
            }
            | Type::TyStruct {
                name:
                    Some(Token {
                        token: Tok::Ident(param_name),
                        ..
                    }),
                ..
            }
            | Type::TyUnion {
                name:
                    Some(Token {
                        token: Tok::Ident(param_name),
                        ..
                    }),
                ..
            } => Ok(param_name),
            _ => panic!("ty {:?} has no token.", ty),
        }
    }

    fn new_var(&mut self, name: String, ty: Type) -> Result<Rc<RefCell<Obj>>> {
        let var = Rc::new(RefCell::new(Obj {
            name: name.clone(),
            ty,
            offset: 0,
            is_local: false,
            init_data: None,
        }));
        self.var_env.enter(self.symbols.symbol(&name), var.clone());
        Ok(var)
    }

    fn new_global_variable(
        &mut self,
        name: String,
        ty: Type,
        init_value: Option<InitData>,
    ) -> Result<Rc<RefCell<Obj>>> {
        let var = self.new_var(name, ty)?;
        var.borrow_mut().is_local = false;
        var.borrow_mut().init_data = init_value;
        self.globals.insert(0, var.clone());
        Ok(var)
    }

    fn new_local_variable(&mut self, name: String, ty: Type) -> Result<Rc<RefCell<Obj>>> {
        let var = self.new_var(name, ty)?;
        var.borrow_mut().is_local = true;
        self.locals.insert(0, var.clone());
        Ok(var)
    }

    fn global_variable(&mut self, pos: usize, basety: Type) -> Result<usize> {
        self.current_pos = pos;
        loop {
            match self.peek()?.token {
                Tok::Semicolon => {
                    eat!(self, Semicolon);
                    break;
                }
                Tok::Comma => {
                    eat!(self, Comma);
                }
                _ => {
                    let ty = self.declarator(basety.clone())?;
                    let var_name = self.get_ident(ty.clone())?;
                    self.new_global_variable(var_name, ty, None)?;
                }
            }
        }
        Ok(self.current_pos)
    }

    /// program = function-definition*
    pub fn parse(&mut self) -> Result<Program> {
        self.globals = vec![];
        loop {
            match self.peek() {
                Ok(Token {
                    token: Tok::EndOfFile,
                    ..
                }) => break,
                _ => {
                    // int ...., 获取基本类型 int
                    let basety = self.declspec()?;
                    // 保存当前的位置
                    let pos = self.current_pos.clone();
                    if self.is_function()? {
                        self.current_pos = self.function(pos, basety)?;
                    } else {
                        self.current_pos = self.global_variable(pos, basety)?;
                    }
                }
            }
        }
        Ok(Program {
            funcs: self.functions.clone(),
            globals: self.globals.clone(),
        })
    }
}
