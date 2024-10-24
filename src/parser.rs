use std::{cell::RefCell, rc::Rc, result};

use crate::{
    ast::{
        self, BinaryOperator, Expr, ExprWithPos, Function, InitData, Obj, Program, Stmt,
        StmtWithPos, UnaryOperator, VarAttr,
    },
    error::Error::{self, UnexpectedToken},
    position::{Pos, WithPos},
    sema::{
        self, add_type, align_to, get_sizeof, is_integer, pointer_to, sema_stmt, Type, WithType,
    },
    symbol::{Strings, Symbols},
    token::{
        Tok::{self, *},
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
    var_attr_env: Symbols<VarAttr>,

    // Points to the function object the parser is currently parsing.
    current_fn: Option<Function>,
}

impl<'a> Parser<'a> {
    pub fn new(tokens: Vec<Token>, symbols: &'a mut Symbols<()>, strings: Rc<Strings>) -> Self {
        let var_env = Symbols::new(Rc::clone(&strings));
        let struct_tag_env = Symbols::new(Rc::clone(&strings));
        let var_attr_env = Symbols::new(Rc::clone(&strings));
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
            var_attr_env,

            current_fn: None,
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
                let return_ty = match self.current_fn.clone() {
                    Some(ast::Function {
                        ty: Type::TyFunc { return_ty, .. },
                        ..
                    }) => return_ty,
                    _ => unreachable!(),
                };
                expr = WithPos::new(
                    WithType::new(
                        Expr::CastExpr {
                            expr: Box::new(expr),
                            ty: *return_ty.clone(),
                        },
                        *return_ty.clone(),
                    ),
                    pos,
                );
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

                self.var_env.begin_scope();

                let tok = self.peek()?.token.clone();
                let init = if self.is_typename(tok)? {
                    let basety = self.declspec(&mut None)?;
                    self.declaration(basety)?
                } else {
                    self.expr_stmt()?
                };

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

                self.var_env.end_scope();

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
    fn declaration(&mut self, basety: Type) -> Result<StmtWithPos> {
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
                    match ty {
                        Type::TyVoid { name } => panic!("{:#?} variable declared void.", name),
                        Type::TyArray { array_len, .. } if array_len < 0 => {
                            panic!("variable has incomplete type.")
                        }
                        _ => (),
                    }
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
                    let basety = self.declspec(&mut None)?;
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
                    let basety = self.declspec(&mut None)?;
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

    /// declspec = ("void" | "_Bool" | "char" | "short" | "int" | "long"
    ///          | "typedef" | "static"
    ///          | struct-decl | union-decl | typedef-name
    ///          | enum-specifier)+
    ///
    /// The order of typenames in a type-specifier doesn't matter. For
    /// example, `int long static` means the same as `static long int`.
    /// That can also be written as `static long` because you can omit
    /// `int` if `long` or `short` are specified. However, something like
    /// `char int` is not a valid type specifier. We have to accept only a
    /// limited combinations of the typenames.
    ///
    /// In this function, we count the number of occurrences of each typename
    /// while keeping the "current" type object that the typenames up
    /// until that point represent. When we reach a non-typename token,
    /// we returns the current type object.
    fn declspec(&mut self, attr: &mut Option<VarAttr>) -> Result<Type> {
        // We use a single integer as counters for all typenames.
        // For example, bits 0 and 1 represents how many times we saw the
        // keyword "void" so far. With this, we can use a switch statement
        // as you can see below.
        enum Counter {
            VOID = 1 << 0,
            BOOL = 1 << 2,
            CHAR = 1 << 4,
            SHORT = 1 << 6,
            INT = 1 << 8,
            LONG = 1 << 10,
            OTHER = 1 << 12,
        }

        let mut counter = 0;
        // typedef t; t为int
        let mut ty = Type::TyInt { name: None };

        let mut typedef_static_count = 0;

        loop {
            let tok = self.peek()?.token.clone();
            if !self.is_typename(tok)? {
                break;
            }
            match self.peek()?.token.clone() {
                Tok::KeywordTypedef => {
                    eat!(self, KeywordTypedef);
                    if attr.is_some() {
                        typedef_static_count += 1;
                        if typedef_static_count > 1 {
                            panic!("typedef and static may not be used together.");
                        }
                        *attr = Some(VarAttr::Typedef { type_def: None });
                        continue;
                    } else {
                        panic!("storage class specifier is not allowed in this context.");
                    }
                }
                Tok::KeywordStatic => {
                    eat!(self, KeywordStatic);
                    if attr.is_some() {
                        typedef_static_count += 1;
                        if typedef_static_count > 1 {
                            panic!("typedef and static may not be used together.");
                        }
                        *attr = Some(VarAttr::Static);
                        continue;
                    } else {
                        panic!("storage class specifier is not allowed in this context.");
                    }
                }
                Tok::KeywordStruct => {
                    if counter > 0 {
                        break;
                    }
                    eat!(self, KeywordStruct);
                    ty = self.struct_decl()?;
                    counter += Counter::OTHER as i32;
                    continue;
                }
                Tok::KeywordUnion => {
                    if counter > 0 {
                        break;
                    }
                    eat!(self, KeywordUnion);
                    ty = self.union_decl()?;
                    counter += Counter::OTHER as i32;
                    continue;
                }
                Tok::KeywordEnum => {
                    if counter > 0 {
                        break;
                    }
                    eat!(self, KeywordEnum);
                    ty = self.enum_specifier()?;
                    counter += Counter::OTHER as i32;
                    continue;
                }
                Tok::Ident(name) => {
                    if counter > 0 {
                        break;
                    }
                    match self.var_attr_env.look(self.symbols.symbol(&name)) {
                        Some(VarAttr::Typedef {
                            type_def: Some(_ty),
                        }) => {
                            ty = _ty.clone();
                            counter += Counter::OTHER as i32;
                            continue;
                        }
                        _ => panic!(),
                    }
                }
                Tok::KeywordVoid => {
                    eat!(self, KeywordVoid);
                    counter += Counter::VOID as i32;
                }
                Tok::KeywordBool => {
                    eat!(self, KeywordBool);
                    counter += Counter::BOOL as i32;
                }
                Tok::KeywordChar => {
                    eat!(self, KeywordChar);
                    counter += Counter::CHAR as i32;
                }
                Tok::KeywordInt => {
                    eat!(self, KeywordInt);
                    counter += Counter::INT as i32;
                }
                Tok::KeywordShort => {
                    eat!(self, KeywordShort);
                    counter += Counter::SHORT as i32;
                }
                Tok::KeywordLong => {
                    eat!(self, KeywordLong);
                    counter += Counter::LONG as i32;
                }
                _ => unreachable!(),
            }

            if counter == Counter::VOID as i32 {
                ty = Type::TyVoid { name: None };
            } else if counter == Counter::BOOL as i32 {
                ty = Type::TyBool { name: None };
            } else if counter == Counter::CHAR as i32 {
                ty = Type::TyChar { name: None };
            } else if counter == Counter::SHORT as i32 {
                ty = Type::TyShort { name: None };
            } else if counter == Counter::SHORT as i32 + Counter::INT as i32 {
                ty = Type::TyShort { name: None };
            } else if counter == Counter::INT as i32 {
                ty = Type::TyInt { name: None };
            } else if counter == Counter::LONG as i32 + Counter::LONG as i32 + Counter::INT as i32 {
                ty = Type::TyLong { name: None };
            } else if counter == Counter::LONG as i32 + Counter::INT as i32 {
                ty = Type::TyLong { name: None };
            } else if counter == Counter::LONG as i32 + Counter::LONG as i32 {
                ty = Type::TyLong { name: None };
            } else if counter == Counter::LONG as i32 {
                ty = Type::TyLong { name: None };
            } else {
                panic!("invalid type.");
            }
        }

        Ok(ty)
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
                    let mut ty2 = self.declspec(&mut None)?;
                    ty2 = self.declarator(ty2)?;

                    // "array of T" is converted to "pointer to T" only in the parameter
                    // context. For example, *argv[] is converted to **argv by this.
                    match ty2 {
                        Type::TyArray {
                            name: old_name,
                            base,
                            array_len: _,
                        } => {
                            ty2 = pointer_to(*base);
                            match ty2 {
                                Type::TyPtr {
                                    name: ref mut new_name,
                                    ..
                                } => {
                                    *new_name = old_name;
                                }
                                _ => (),
                            }
                        }
                        _ => (),
                    }

                    params.push(ty2);
                }
            }
        }

        let node = Type::TyFunc {
            name: None,
            params,
            return_ty: Box::new(ty),
        };
        Ok(node)
    }

    /// type-suffix = "(" func-params
    ///             | "[" array-dimensions
    ///             | ε
    fn type_suffix(&mut self, ty: Type) -> Result<Type> {
        match self.peek()?.token {
            Tok::LeftParen => {
                eat!(self, LeftParen);
                self.func_params(ty)
            }
            Tok::LeftBracket => {
                eat!(self, LeftBracket);
                return self.array_dimensions(ty);
            }
            _ => Ok(ty),
        }
    }

    /// declarator = "*"* ("(" ident ")" | "(" declarator ")" | ident) type-suffix
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
            Tok::LeftParen => {
                eat!(self, LeftParen);
                match self.peek()?.token {
                    Tok::Ident(..) => match self.peek_next_one()?.token {
                        Tok::RightParen => {
                            let tok = self.token()?;
                            eat!(self, RightParen);
                            let mut ty = self.type_suffix(ty)?;
                            match ty {
                                Type::TyInt { ref mut name }
                                | Type::TyVoid { ref mut name }
                                | Type::TyBool { ref mut name }
                                | Type::TyChar { ref mut name }
                                | Type::TyShort { ref mut name }
                                | Type::TyLong { ref mut name }
                                | Type::TyPtr { ref mut name, .. }
                                | Type::TyFunc { ref mut name, .. }
                                | Type::TyArray { ref mut name, .. }
                                | Type::TyEnum { ref mut name, .. }
                                | Type::TyStruct { ref mut name, .. }
                                | Type::TyUnion { ref mut name, .. } => *name = Some(tok.clone()),
                                _ => (),
                            }
                            return Ok(ty);
                        }
                        _ => {
                            let mut _ty = self.declarator(ty.clone())?;
                            eat!(self, RightParen);
                            let ty = self.type_suffix(ty)?;
                            match _ty {
                                Type::TyPtr { ref mut base, .. }
                                | Type::TyArray { ref mut base, .. } => {
                                    *base = Box::new(ty);
                                }
                                _ => panic!(),
                            }
                            return Ok(_ty);
                        }
                    },
                    _ => {
                        let mut _ty = self.declarator(ty.clone())?;
                        eat!(self, RightParen);
                        let ty = self.type_suffix(ty)?;
                        match _ty {
                            Type::TyPtr { ref mut base, .. } => {
                                *base = Box::new(ty);
                            }
                            _ => panic!(),
                        }
                        return Ok(_ty);
                    }
                }
            }
            Tok::Ident(..) => {
                let tok = self.token()?;
                ty = self.type_suffix(ty)?;
                match ty {
                    Type::TyInt { ref mut name }
                    | Type::TyVoid { ref mut name }
                    | Type::TyBool { ref mut name }
                    | Type::TyChar { ref mut name }
                    | Type::TyShort { ref mut name }
                    | Type::TyLong { ref mut name }
                    | Type::TyPtr { ref mut name, .. }
                    | Type::TyFunc { ref mut name, .. }
                    | Type::TyArray { ref mut name, .. }
                    | Type::TyEnum { ref mut name, .. }
                    | Type::TyStruct { ref mut name, .. }
                    | Type::TyUnion { ref mut name, .. } => *name = Some(tok.clone()),
                    _ => (),
                }
            }
            _ => panic!("expected a variable name, but got {:?}", self.peek()?),
        }

        Ok(ty)
    }

    fn is_typename(&mut self, tok: Tok) -> Result<bool> {
        match tok {
            Tok::KeywordLong
            | Tok::KeywordInt
            | Tok::KeywordEnum
            | Tok::KeywordBool
            | Tok::KeywordChar
            | Tok::KeywordShort
            | Tok::KeywordStruct
            | Tok::KeywordUnion
            | Tok::KeywordTypedef
            | Tok::KeywordStatic
            | Tok::KeywordVoid => Ok(true),
            Tok::Ident(name) => {
                let symbol = self.symbols.symbol(&name);
                let t = self.var_attr_env.look(symbol);
                match t {
                    Some(VarAttr::Typedef { .. }) => Ok(true),
                    _ => Ok(false),
                }
            }
            _ => Ok(false),
        }
    }

    /// compound-stmt = (declaration | stmt)* "}"
    fn compound_stmt(&mut self) -> Result<StmtWithPos> {
        let mut stmts = vec![];

        self.var_env.begin_scope();
        self.struct_tag_env.begin_scope();
        self.var_attr_env.begin_scope();
        loop {
            match self.peek()?.token {
                Tok::RightBrace => {
                    eat!(self, RightBrace);
                    break;
                }
                _ => {
                    // { typedef int t; t t=1; t; }
                    let tok = self.peek()?.token.clone();
                    match tok.clone() {
                        Tok::Ident(name) => {
                            let sym = self.symbols.symbol(&name);
                            if self.var_env.look(sym).is_some() {
                                let mut stmt = self.stmt()?;
                                sema_stmt(&mut stmt);
                                stmts.push(stmt);
                                continue;
                            }
                        }
                        _ => (),
                    }
                    if self.is_typename(tok)? {
                        let mut attr = Some(VarAttr::Placeholder);
                        let basety = self.declspec(&mut attr)?;
                        match attr {
                            Some(VarAttr::Typedef { .. }) => {
                                self.parse_typedef(basety)?;
                                continue;
                            }
                            _ => (),
                        }
                        let mut declaration = self.declaration(basety)?;
                        sema_stmt(&mut declaration);
                        stmts.push(declaration);
                    } else {
                        let mut stmt = self.stmt()?;
                        sema_stmt(&mut stmt);
                        stmts.push(stmt);
                    }
                }
            }
        }
        self.var_attr_env.end_scope();
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

    /// Convert `A op= B` to `tmp = &A, *tmp = *tmp op B`
    /// where tmp is a fresh pointer variable.
    fn to_assign(&mut self, binary: ExprWithPos) -> Result<ExprWithPos> {
        match binary.node.node {
            Expr::Binary {
                mut left,
                op,
                mut right,
            } => {
                add_type(&mut left);
                add_type(&mut right);

                let var =
                    self.new_local_variable("".to_string(), pointer_to(left.node.ty.clone()))?;

                // tmp = &A
                let expr1 = WithPos::new(
                    WithType::new(
                        Expr::Assign {
                            l_value: Box::new(WithPos::new(
                                WithType::new(
                                    Expr::Variable { obj: var.clone() },
                                    var.borrow().ty.clone(),
                                ),
                                left.pos,
                            )),
                            r_value: Box::new(WithPos::new(
                                WithType::new(
                                    Expr::Addr { expr: left.clone() },
                                    var.borrow().ty.clone(),
                                ),
                                left.pos,
                            )),
                        },
                        var.borrow().ty.clone(),
                    ),
                    left.pos,
                );

                // *tmp = *tmp op B
                let expr2 = WithPos::new(
                    WithType::new(
                        Expr::Assign {
                            // *tmp
                            l_value: Box::new(WithPos::new(
                                WithType::new(
                                    Expr::Deref {
                                        expr: Box::new(WithPos::new(
                                            WithType::new(
                                                Expr::Variable { obj: var.clone() },
                                                var.borrow().ty.clone(),
                                            ),
                                            left.pos,
                                        )),
                                    },
                                    left.node.ty.clone(),
                                ),
                                left.pos,
                            )),
                            r_value: Box::new(WithPos::new(
                                WithType::new(
                                    Expr::Binary {
                                        left: Box::new(WithPos::new(
                                            WithType::new(
                                                Expr::Deref {
                                                    expr: Box::new(WithPos::new(
                                                        WithType::new(
                                                            Expr::Variable { obj: var.clone() },
                                                            var.borrow().ty.clone(),
                                                        ),
                                                        left.pos,
                                                    )),
                                                },
                                                left.node.ty.clone(),
                                            ),
                                            left.pos,
                                        )),
                                        op,
                                        right: right.clone(),
                                    },
                                    left.node.ty.clone(),
                                ),
                                left.pos,
                            )),
                        },
                        left.node.ty.clone(),
                    ),
                    left.pos,
                );

                let node = WithPos::new(
                    WithType::new(
                        Expr::CommaExpr {
                            left: Box::new(expr1.clone()),
                            right: Box::new(expr2.clone()),
                        },
                        expr2.node.ty.clone(),
                    ),
                    left.pos,
                );

                Ok(node)
            }
            _ => panic!(),
        }
    }

    /// assign    = logor (assign-op assign)?
    /// assign-op = "=" | "+=" | "-=" | "*=" | "/=" | "%=" | "&=" | "|=" | "^="
    fn assign(&mut self) -> Result<ExprWithPos> {
        let mut expr = self.logor()?;
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
            Tok::PlusEqual => {
                let pos = eat!(self, PlusEqual);
                let rhs = self.assign()?;
                expr = self.new_add(expr, rhs, pos)?;
                expr = self.to_assign(expr)?;
            }
            Tok::MinusEqual => {
                let pos = eat!(self, MinusEqual);
                let rhs = self.assign()?;
                expr = self.new_sub(expr, rhs, pos)?;
                expr = self.to_assign(expr)?;
            }
            Tok::StarEqual => {
                let pos = eat!(self, StarEqual);
                let rhs = self.assign()?;
                expr = WithPos::new(
                    WithType::new(
                        Expr::Binary {
                            left: Box::new(expr.clone()),
                            op: WithPos::new(ast::BinaryOperator::Mul, pos),
                            right: Box::new(rhs),
                        },
                        expr.node.ty,
                    ),
                    pos,
                );
                expr = self.to_assign(expr)?;
            }
            Tok::SlashEqual => {
                let pos = eat!(self, SlashEqual);
                let rhs = self.assign()?;
                expr = WithPos::new(
                    WithType::new(
                        Expr::Binary {
                            left: Box::new(expr.clone()),
                            op: WithPos::new(ast::BinaryOperator::Div, pos),
                            right: Box::new(rhs),
                        },
                        expr.node.ty,
                    ),
                    pos,
                );
                expr = self.to_assign(expr)?;
            }
            Tok::PercentEqual => {
                let pos = eat!(self, PercentEqual);
                let rhs = self.assign()?;
                expr = WithPos::new(
                    WithType::new(
                        Expr::Binary {
                            left: Box::new(expr.clone()),
                            op: WithPos::new(ast::BinaryOperator::Mod, pos),
                            right: Box::new(rhs),
                        },
                        expr.node.ty,
                    ),
                    pos,
                );
                expr = self.to_assign(expr)?;
            }
            Tok::AmpEqual => {
                let pos = eat!(self, AmpEqual);
                let rhs = self.assign()?;
                expr = WithPos::new(
                    WithType::new(
                        Expr::Binary {
                            left: Box::new(expr.clone()),
                            op: WithPos::new(ast::BinaryOperator::BitAnd, pos),
                            right: Box::new(rhs),
                        },
                        expr.node.ty,
                    ),
                    pos,
                );
                expr = self.to_assign(expr)?;
            }
            Tok::BarEqual => {
                let pos = eat!(self, BarEqual);
                let rhs = self.assign()?;
                expr = WithPos::new(
                    WithType::new(
                        Expr::Binary {
                            left: Box::new(expr.clone()),
                            op: WithPos::new(ast::BinaryOperator::BitOr, pos),
                            right: Box::new(rhs),
                        },
                        expr.node.ty,
                    ),
                    pos,
                );
                expr = self.to_assign(expr)?;
            }
            Tok::HatEqual => {
                let pos = eat!(self, HatEqual);
                let rhs = self.assign()?;
                expr = WithPos::new(
                    WithType::new(
                        Expr::Binary {
                            left: Box::new(expr.clone()),
                            op: WithPos::new(ast::BinaryOperator::BitXor, pos),
                            right: Box::new(rhs),
                        },
                        expr.node.ty,
                    ),
                    pos,
                );
                expr = self.to_assign(expr)?;
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
                    let right = self.mul()?;
                    expr = self.new_sub(expr, right, pos)?;
                }
                _ => break,
            }
        }
        Ok(expr)
    }

    /// logor = logand ("||" logand)*
    fn logor(&mut self) -> Result<ExprWithPos> {
        let mut node = self.logand()?;

        loop {
            match self.peek()?.token {
                Tok::BarBar => {
                    let pos = eat!(self, BarBar);
                    let rhs = self.logand()?;
                    node = WithPos::new(
                        WithType::new(
                            Expr::Binary {
                                left: Box::new(node),
                                op: WithPos::new(ast::BinaryOperator::LogOr, pos),
                                right: Box::new(rhs),
                            },
                            Type::TyPlaceholder,
                        ),
                        pos,
                    );
                }
                _ => break,
            }
        }

        Ok(node)
    }

    /// logand = bitor ("&&" bitor)*
    fn logand(&mut self) -> Result<ExprWithPos> {
        let mut node = self.bitor()?;

        loop {
            match self.peek()?.token {
                Tok::AmpAmp => {
                    let pos = eat!(self, AmpAmp);
                    let rhs = self.bitor()?;
                    node = WithPos::new(
                        WithType::new(
                            Expr::Binary {
                                left: Box::new(node),
                                op: WithPos::new(ast::BinaryOperator::LogAnd, pos),
                                right: Box::new(rhs),
                            },
                            Type::TyPlaceholder,
                        ),
                        pos,
                    );
                }
                _ => break,
            }
        }

        Ok(node)
    }

    /// bitor = bitxor ("|" bitxor)*
    fn bitor(&mut self) -> Result<ExprWithPos> {
        let mut node = self.bitxor()?;

        loop {
            match self.peek()?.token {
                Tok::Bar => {
                    let pos = eat!(self, Bar);
                    let rhs = self.bitxor()?;
                    node = WithPos::new(
                        WithType::new(
                            Expr::Binary {
                                left: Box::new(node),
                                op: WithPos::new(ast::BinaryOperator::BitOr, pos),
                                right: Box::new(rhs),
                            },
                            Type::TyPlaceholder,
                        ),
                        pos,
                    );
                }
                _ => break,
            }
        }

        Ok(node)
    }

    /// bitxor = bitand ("^" bitand)*
    fn bitxor(&mut self) -> Result<ExprWithPos> {
        let mut node = self.bitand()?;

        loop {
            match self.peek()?.token {
                Tok::Hat => {
                    let pos = eat!(self, Hat);
                    let rhs = self.bitand()?;
                    node = WithPos::new(
                        WithType::new(
                            Expr::Binary {
                                left: Box::new(node),
                                op: WithPos::new(ast::BinaryOperator::BitXor, pos),
                                right: Box::new(rhs),
                            },
                            Type::TyPlaceholder,
                        ),
                        pos,
                    );
                }
                _ => break,
            }
        }

        Ok(node)
    }

    /// bitand = equality ("&" equality)*
    fn bitand(&mut self) -> Result<ExprWithPos> {
        let mut node = self.equality()?;

        loop {
            match self.peek()?.token {
                Tok::Amp => {
                    let pos = eat!(self, Amp);
                    let rhs = self.equality()?;
                    node = WithPos::new(
                        WithType::new(
                            Expr::Binary {
                                left: Box::new(node),
                                op: WithPos::new(ast::BinaryOperator::BitAnd, pos),
                                right: Box::new(rhs),
                            },
                            Type::TyPlaceholder,
                        ),
                        pos,
                    );
                }
                _ => break,
            }
        }

        Ok(node)
    }

    /// mul = cast ("*" cast | "/" cast)*
    fn mul(&mut self) -> Result<ExprWithPos> {
        let mut expr = self.cast()?;

        loop {
            let op = match self.peek_token() {
                Ok(&Tok::Star) => WithPos::new(BinaryOperator::Mul, eat!(self, Star)),
                Ok(&Tok::Slash) => WithPos::new(BinaryOperator::Div, eat!(self, Slash)),
                Ok(&Tok::Percent) => WithPos::new(BinaryOperator::Mod, eat!(self, Percent)),
                _ => break,
            };
            let mut right = Box::new(self.cast()?);
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

    /// unary = ("+" | "-" | "*" | "&" | "!" | "~") cast
    ///       | ("++" | "--") unary
    ///       | primary
    fn unary(&mut self) -> Result<ExprWithPos> {
        match self.peek()?.token {
            Plus => {
                eat!(self, Plus);
                Ok(self.cast()?)
            }
            Minus => {
                let op = WithPos::new(UnaryOperator::Neg, eat!(self, Minus));
                let expr = self.cast()?;
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
                let mut expr = self.cast()?;
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
                let mut expr = self.cast()?;
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
            Bang => {
                let op = WithPos::new(UnaryOperator::Not, eat!(self, Bang));
                let expr = self.cast()?;
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
            Tilde => {
                let op = WithPos::new(UnaryOperator::BitNot, eat!(self, Tilde));
                let expr = self.cast()?;
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
            // read ++i as i+=1
            PlusPlus => {
                let pos = eat!(self, PlusPlus);
                let i = self.unary()?;
                let one = WithPos::new(
                    WithType::new(Expr::Number { value: 1 }, i.node.ty.clone()),
                    pos,
                );
                let binary = self.new_add(i, one, pos)?;
                self.to_assign(binary)
            }
            // read --i as i-=1
            MinusMinus => {
                let pos = eat!(self, MinusMinus);
                let i = self.unary()?;
                let one = WithPos::new(
                    WithType::new(Expr::Number { value: 1 }, i.node.ty.clone()),
                    pos,
                );
                let binary = self.new_sub(i, one, pos)?;
                self.to_assign(binary)
            }
            _ => self.postfix(),
        }
    }

    /// cast = "(" type-name ")" cast | unary
    fn cast(&mut self) -> Result<ExprWithPos> {
        match self.peek()?.token {
            Tok::LeftParen => {
                let next_one_tok = self.peek_next_one()?.token.clone();
                if self.is_typename(next_one_tok)? {
                    let pos = eat!(self, LeftParen);
                    let ty = self.typename()?;
                    eat!(self, RightParen);
                    let node = WithPos::new(
                        WithType::new(
                            Expr::CastExpr {
                                expr: Box::new(self.cast()?),
                                ty: ty.clone(),
                            },
                            ty,
                        ),
                        pos,
                    );
                    Ok(node)
                } else {
                    self.unary()
                }
            }
            _ => self.unary(),
        }
    }

    fn new_sub(
        &mut self,
        mut lhs: ExprWithPos,
        mut rhs: ExprWithPos,
        pos: Pos,
    ) -> Result<ExprWithPos> {
        add_type(&mut lhs);
        add_type(&mut rhs);

        match (lhs.node.ty.clone(), rhs.node.ty.clone()) {
            // num - num
            _ if is_integer(lhs.node.ty.clone()) && is_integer(rhs.node.ty.clone()) => {
                lhs = WithPos::new(
                    WithType::new(
                        Expr::Binary {
                            left: Box::new(lhs.clone()),
                            op: WithPos::new(ast::BinaryOperator::Sub, pos),
                            right: Box::new(rhs),
                        },
                        lhs.node.ty.clone(),
                    ),
                    pos,
                );
            }
            // ptr - num
            (Type::TyPtr { base, .. }, Type::TyInt { .. } | Type::TyLong { .. }) => {
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
                                    Type::TyLong { name: None },
                                ),
                                pos,
                            )),
                        },
                        Type::TyLong { name: None },
                    ),
                    pos,
                );
                add_type(&mut rhs);
                lhs = WithPos::new(
                    WithType::new(
                        Expr::Binary {
                            left: Box::new(lhs.clone()),
                            op: WithPos::new(ast::BinaryOperator::Sub, pos),
                            right: Box::new(rhs),
                        },
                        lhs.node.ty.clone(),
                    ),
                    pos,
                );
            }
            // ptr - ptr, which returns how many elements are between the two.
            (Type::TyPtr { base, .. }, Type::TyPtr { .. }) => {
                let left = WithPos::new(
                    WithType::new(
                        Expr::Binary {
                            left: Box::new(lhs),
                            op: WithPos::new(ast::BinaryOperator::Sub, pos),
                            right: Box::new(rhs),
                        },
                        Type::TyLong { name: None },
                    ),
                    pos,
                );
                lhs = WithPos::new(
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

        Ok(lhs)
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
            // ptr + num
            (
                Type::TyPtr { base, .. } | Type::TyArray { base, .. },
                Type::TyInt { .. } | Type::TyLong { .. },
            ) => {
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
                                    Type::TyLong { name: None },
                                ),
                                pos,
                            )),
                        },
                        Type::TyLong { name: None },
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
            (
                Type::TyInt { .. } | Type::TyLong { .. },
                Type::TyPtr { base, .. } | Type::TyArray { base, .. },
            ) => {
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
                                    Type::TyLong { name: None },
                                ),
                                pos,
                            )),
                        },
                        Type::TyLong { name: None },
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
            // num + num
            _ if is_integer(lhs.node.ty.clone()) && is_integer(rhs.node.ty.clone()) => {
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

    /// Convert A++ to `(typeof A)((A += 1) - 1)`
    /// Convert A-- to `(typeof A)((A -= 1) + 1)`
    fn new_inc_dec(&mut self, mut node: ExprWithPos, addend: i32) -> Result<ExprWithPos> {
        add_type(&mut node);

        let pos = self.peek()?.pos;

        // 1
        let positive_one = WithPos::new(
            WithType::new(
                Expr::Number {
                    value: addend as i64,
                },
                Type::TyLong { name: None },
            ),
            pos,
        );

        // -1
        let negative_one = WithPos::new(
            WithType::new(
                Expr::Number {
                    value: -addend as i64,
                },
                Type::TyLong { name: None },
            ),
            pos,
        );

        // A += 1
        // A + 1
        let a_plus_one = self.new_add(node.clone(), positive_one, pos)?;
        // A = A + 1
        let a = self.to_assign(a_plus_one)?;
        let e = self.new_add(a, negative_one, pos)?;

        // cast
        let c = WithPos::new(
            WithType::new(
                Expr::CastExpr {
                    expr: Box::new(e),
                    ty: node.node.ty.clone(),
                },
                node.node.ty.clone(),
            ),
            pos,
        );

        Ok(c)
    }

    /// postfix = primary ("[" expr "]" | "." ident | "->" ident | "++" | "--")*
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
                Tok::PlusPlus => {
                    eat!(self, PlusPlus);
                    node = self.new_inc_dec(node, 1)?;
                }
                Tok::MinusMinus => {
                    eat!(self, MinusMinus);
                    node = self.new_inc_dec(node, -1)?;
                }
                _ => break,
            }
        }

        Ok(node)
    }

    /// funcall = ident "(" (assign ("," assign)*)? ")"
    fn funcall(&mut self, funname: String) -> Result<ExprWithPos> {
        let mut func_found = None;
        for f in &self.functions {
            if f.name == funname {
                func_found = Some(f.clone());
                break;
            }
        }

        if func_found.is_none() {
            return Err(Error::ImplicitDeclarationOfFunction {
                name: funname,
                pos: self.peek()?.pos,
            });
        }

        let pos = eat!(self, LeftParen);
        let mut args = vec![];

        let params_ty = match func_found.clone().unwrap().ty.clone() {
            Type::TyFunc { params, .. } => params,
            _ => unreachable!(),
        };

        let mut i = 0;
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
                    args.push(match arg_exp.node.ty.clone() {
                        Type::TyStruct { .. } | Type::TyUnion { .. } => {
                            panic!("passing struct or union is not supported yet.")
                        }
                        _ => WithPos::new(
                            WithType::new(
                                Expr::CastExpr {
                                    expr: Box::new(arg_exp.clone()),
                                    ty: match params_ty.get(i).clone() {
                                        Some(t) => t.clone(),
                                        _ => arg_exp.node.ty.clone(),
                                    },
                                },
                                match params_ty.get(i).clone() {
                                    Some(t) => t.clone(),
                                    _ => arg_exp.node.ty.clone(),
                                },
                            ),
                            pos,
                        ),
                    });
                    i += 1;
                }
            }
        }

        let node = WithPos::new(
            WithType::new(
                Expr::FunctionCall {
                    name: funname,
                    args,
                },
                match func_found.unwrap().ty {
                    Type::TyFunc { return_ty, .. } => *return_ty,
                    _ => unreachable!(),
                },
            ),
            pos,
        );
        Ok(node)
    }

    /// abstract-declarator = "*"* ("(" abstract-declarator ")")? type-suffix
    fn abstract_declarator(&mut self, mut ty: Type) -> Result<Type> {
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
            Tok::LeftParen => {
                eat!(self, LeftParen);
                self.abstract_declarator(Type::TyPlaceholder)?;
                eat!(self, RightParen);
                ty = self.type_suffix(ty)?;
                Ok(Type::TyPtr {
                    base: Box::new(ty),
                    name: None,
                })
            }
            _ => self.type_suffix(ty),
        }
    }

    /// type-name = declspec abstract-declarator
    fn typename(&mut self) -> Result<Type> {
        let ty = self.declspec(&mut None)?;
        self.abstract_declarator(ty)
    }

    /// primary = "(" "{" stmt+ "}" ")"
    ///         | "(" expr ")"
    ///         | "sizeof" "(" type-name ")"
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
                let mut node = WithPos::new(
                    WithType::new(Expr::Number { value }, Type::TyLong { name: None }),
                    pos,
                );
                add_type(&mut node);
                Ok(node)
            }
            Tok::KeywordSizeof => {
                let pos = eat!(self, KeywordSizeof);
                match self.peek()?.token {
                    Tok::LeftParen => {
                        let tok = self.peek_next_one()?.token.clone();
                        // sizeof(int **)
                        if self.is_typename(tok)? {
                            eat!(self, LeftParen);
                            let ty = self.typename()?;
                            eat!(self, RightParen);
                            Ok(WithPos::new(
                                WithType::new(
                                    Expr::Number {
                                        value: get_sizeof(ty) as i64,
                                    },
                                    Type::TyInt { name: None },
                                ),
                                pos,
                            ))
                        }
                        // sizeof(x)
                        else {
                            let mut node = self.unary()?;
                            add_type(&mut node);
                            node = WithPos::new(
                                WithType::new(
                                    Expr::Number {
                                        value: get_sizeof(node.node.ty.clone()) as i64,
                                    },
                                    Type::TyInt { name: None },
                                ),
                                pos,
                            );
                            Ok(node)
                        }
                    }
                    // sizeof x
                    _ => {
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
                }
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
                    match _ty {
                        Type::TyEnum { .. } => {
                            if let Some(InitData::IntInitData(i)) = var.borrow().init_data {
                                return Ok(WithPos::new(
                                    WithType::new(Expr::Number { value: i as i64 }, _ty),
                                    pos,
                                ));
                            } else {
                                return Ok(WithPos::new(
                                    WithType::new(Expr::Variable { obj: var.clone() }, _ty),
                                    pos,
                                ));
                            }
                        }
                        _ => {
                            return Ok(WithPos::new(
                                WithType::new(Expr::Variable { obj: var.clone() }, _ty),
                                pos,
                            ));
                        }
                    }
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

    fn peek_next_one(&mut self) -> std::result::Result<&Token, &Error> {
        if let Some(tok) = self.tokens.get(self.current_pos + 1) {
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

    fn function(&mut self, pos: usize, basety: Type, attr: &Option<VarAttr>) -> Result<usize> {
        self.current_pos = pos;

        let ty = self.declarator(basety)?;

        match ty.clone() {
            Type::TyFunc { ref mut name, .. } => *name = Some(self.get_ident_token(ty.clone())?),
            _ => panic!("{:?} must be function name.", self.get_ident(ty)?),
        }

        let is_definition = match self.peek()?.token {
            Tok::Semicolon => {
                eat!(self, Semicolon);
                false
            }
            _ => true,
        };

        let is_static = match attr {
            Some(VarAttr::Static) => true,
            _ => false,
        };

        self.locals = Vec::new();

        self.var_env.begin_scope();
        self.struct_tag_env.begin_scope();
        self.var_attr_env.begin_scope();

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
                let pos = self.peek()?.pos.clone();
                // 为了处理fib这样的递归调用，需要先把函数声明记录下来。
                self.functions.push(ast::Function {
                    name: ident.clone(),
                    params: vec![],
                    body: WithPos::new(Stmt::NullStmt, pos),
                    locals: vec![],
                    stack_size: 0,
                    is_definition: false,
                    ty: ty.clone(),
                    is_static,
                });

                self.current_fn = Some(ast::Function {
                    name: ident.clone(),
                    params: vec![],
                    body: WithPos::new(Stmt::NullStmt, pos),
                    locals: vec![],
                    stack_size: 0,
                    is_definition: false,
                    ty: ty.clone(),
                    is_static,
                });

                for p in params {
                    let param_name = self.get_ident(p.clone())?;
                    self.new_local_variable(param_name, p)?;
                }
            }
            _ => panic!("ident must have a type."),
        }

        // 先将函数的参数拷贝一份，因为后面在解析函数体时会加入新的局部变量
        let params = self.locals.clone();

        let body;
        if is_definition {
            eat!(self, LeftBrace);
            body = self.compound_stmt()?;
        } else {
            body = WithPos::new(Stmt::NullStmt, self.peek()?.pos);
        }

        self.var_attr_env.end_scope();
        self.struct_tag_env.end_scope();
        self.var_env.end_scope();

        self.functions.push(ast::Function {
            name: ident,
            params,
            body,
            locals: self.locals.clone(),
            stack_size: 0,
            is_definition: is_definition,
            ty,
            is_static,
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
            | Type::TyBool { name: Some(t), .. }
            | Type::TyChar { name: Some(t), .. }
            | Type::TyShort { name: Some(t), .. }
            | Type::TyLong { name: Some(t), .. }
            | Type::TyPtr { name: Some(t), .. }
            | Type::TyStruct { name: Some(t), .. }
            | Type::TyUnion { name: Some(t), .. }
            | Type::TyEnum { name: Some(t), .. }
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
            }
            | Type::TyBool {
                name:
                    Some(Token {
                        token: Tok::Ident(param_name),
                        ..
                    }),
                ..
            }
            | Type::TyEnum {
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

    fn parse_typedef(&mut self, basety: Type) -> Result<()> {
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
                    let ident = self.get_ident(ty.clone())?;
                    self.var_attr_env.enter(
                        self.symbols.symbol(&ident),
                        VarAttr::Typedef { type_def: Some(ty) },
                    );
                }
            }
        }
        Ok(())
    }

    // enum-specifier = ident? "{" enum-list? "}"
    //                | ident ("{" enum-list? "}")?
    //
    // enum-list      = ident ("=" num)? ("," ident ("=" num)?)*
    fn enum_specifier(&mut self) -> Result<Type> {
        // read a enum tag
        match self.peek()?.token.clone() {
            Tok::Ident(..) => {
                let ident_tok = self.peek()?.clone();
                let ident;
                eat!(self, Ident, ident);
                match self.peek()?.token.clone() {
                    Tok::LeftBrace => {
                        eat!(self, LeftBrace);
                        // read an enum-list
                        let mut val = 0;
                        loop {
                            match self.peek()?.token {
                                Tok::RightBrace => {
                                    eat!(self, RightBrace);
                                    break;
                                }
                                Tok::Comma => {
                                    eat!(self, Comma);
                                }
                                _ => {
                                    let name;
                                    eat!(self, Ident, name);
                                    match self.peek()?.token {
                                        Tok::Equal => {
                                            eat!(self, Equal);
                                            match self.peek()?.token {
                                                Tok::Number(..) => {
                                                    let i;
                                                    eat!(self, Number, i);
                                                    val = i as i32;
                                                }
                                                _ => panic!(),
                                            }
                                        }
                                        _ => (),
                                    }

                                    // 将enum添加到`变量`作用域
                                    let enum_var = Rc::new(RefCell::new(Obj {
                                        name: name.clone(),
                                        offset: 0,
                                        is_local: false,
                                        init_data: Some(InitData::IntInitData(val)),
                                        ty: Type::TyEnum {
                                            name: Some(ident_tok.clone()),
                                        },
                                    }));
                                    val += 1;
                                    self.var_env.enter(self.symbols.symbol(&name), enum_var);
                                }
                            }
                        }

                        self.struct_tag_env.enter(
                            self.symbols.symbol(&ident),
                            Type::TyEnum {
                                name: Some(ident_tok.clone()),
                            },
                        );

                        Ok(Type::TyEnum {
                            name: Some(ident_tok.clone()),
                        })
                    }
                    _ => {
                        if let Some(ty) = self.struct_tag_env.look(self.symbols.symbol(&ident)) {
                            match ty {
                                Type::TyEnum { .. } => {
                                    return Ok(ty.clone());
                                }
                                _ => panic!(),
                            }
                        } else {
                            panic!()
                        }
                    }
                }
            }
            _ => match self.peek()?.token {
                Tok::LeftBrace => {
                    eat!(self, LeftBrace);
                    // read an enum-list
                    let mut val = 0;
                    loop {
                        match self.peek()?.token {
                            Tok::RightBrace => {
                                eat!(self, RightBrace);
                                break;
                            }
                            Tok::Comma => {
                                eat!(self, Comma);
                            }
                            _ => {
                                let name;
                                eat!(self, Ident, name);
                                match self.peek()?.token {
                                    Tok::Equal => {
                                        eat!(self, Equal);
                                        match self.peek()?.token {
                                            Tok::Number(..) => {
                                                let i;
                                                eat!(self, Number, i);
                                                val = i as i32;
                                            }
                                            _ => panic!(),
                                        }
                                    }
                                    _ => (),
                                }

                                // 将enum添加到`变量`作用域
                                let enum_var = Rc::new(RefCell::new(Obj {
                                    name: name.clone(),
                                    offset: 0,
                                    is_local: false,
                                    init_data: Some(InitData::IntInitData(val)),
                                    ty: Type::TyEnum { name: None },
                                }));
                                val += 1;
                                self.var_env.enter(self.symbols.symbol(&name), enum_var);
                            }
                        }
                    }

                    Ok(Type::TyEnum { name: None })
                }
                _ => panic!(),
            },
        }
    }

    /// array-dimensions = num? "]" type-suffix
    fn array_dimensions(&mut self, mut ty: Type) -> Result<Type> {
        match self.peek()?.token {
            Tok::RightBracket => {
                eat!(self, RightBracket);
                ty = self.type_suffix(ty)?;
                Ok(Type::TyArray {
                    name: None,
                    base: Box::new(ty),
                    array_len: -1,
                })
            }
            _ => {
                let sz;
                eat!(self, Number, sz);
                eat!(self, RightBracket);
                ty = self.type_suffix(ty)?;
                Ok(Type::TyArray {
                    name: None,
                    base: Box::new(ty),
                    array_len: sz as i32,
                })
            }
        }
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
                    let mut attr = Some(VarAttr::Placeholder);
                    let basety = self.declspec(&mut attr)?;
                    match attr {
                        Some(VarAttr::Typedef { .. }) => {
                            self.parse_typedef(basety)?;
                            continue;
                        }
                        _ => (),
                    }
                    // 保存当前的位置
                    let pos = self.current_pos.clone();
                    if self.is_function()? {
                        self.current_pos = self.function(pos, basety, &attr)?;
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
