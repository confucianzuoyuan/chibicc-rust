use std::{cell::RefCell, collections::HashMap, rc::Rc, result};

use crate::{
    ast::{
        self, BinaryOperator, Expr, ExprWithPos, FunctionDefinition, InitData, InitDesg,
        Initializer, Obj, Program, Relocation, Stmt, StmtWithPos, UnaryOperator, VarAttr,
    },
    error::Error::{self, UnexpectedToken},
    position::{Pos, WithPos},
    sema::{self, add_type, align_to, pointer_to, sema_stmt, Ty, Type, WithType},
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
    functions: Vec<FunctionDefinition>,
    unique_name_count: i32,

    var_env: Symbols<Rc<RefCell<Obj>>>,
    struct_union_tag_env: Symbols<Type>,
    typedef_env: Symbols<Type>,

    // Points to the function object the parser is currently parsing.
    current_fn: Option<FunctionDefinition>,

    goto_labels: HashMap<String, String>,
    break_label: Option<String>,
    continue_label: Option<String>,

    case_stmts: Vec<StmtWithPos>,
    default_case_stmt: Option<StmtWithPos>,
}

impl<'a> Parser<'a> {
    pub fn new(tokens: Vec<Token>, symbols: &'a mut Symbols<()>, strings: Rc<Strings>) -> Self {
        let var_env = Symbols::new(Rc::clone(&strings));
        let struct_union_tag_env = Symbols::new(Rc::clone(&strings));
        let typedef_env = Symbols::new(Rc::clone(&strings));
        Parser {
            tokens,
            current_pos: 0,
            symbols,
            locals: Vec::new(),
            globals: Vec::new(),
            functions: Vec::new(),
            unique_name_count: 0,

            var_env,
            struct_union_tag_env,
            typedef_env,

            current_fn: None,

            goto_labels: HashMap::new(),
            break_label: None,
            continue_label: None,

            case_stmts: vec![],
            default_case_stmt: None,
        }
    }

    fn begin_scope(&mut self) {
        self.var_env.begin_scope();
        self.struct_union_tag_env.begin_scope();
        self.typedef_env.begin_scope();
    }

    fn end_scope(&mut self) {
        self.typedef_env.end_scope();
        self.struct_union_tag_env.end_scope();
        self.var_env.end_scope();
    }

    fn new_unique_name(&mut self) -> String {
        let unique_name = format!(".L..{}", self.unique_name_count);
        self.unique_name_count += 1;
        unique_name
    }

    fn is_end(&mut self) -> Result<bool> {
        if self.peek()?.token == RightBrace {
            Ok(true)
        } else if self.peek()?.token == Comma && self.peek_next_one()?.token == RightBrace {
            Ok(true)
        } else {
            Ok(false)
        }
    }

    fn consume_end(&mut self) -> Result<bool> {
        if self.peek()?.token == RightBrace {
            eat!(self, RightBrace);
            return Ok(true);
        }

        if self.peek()?.token == Comma && self.peek_next_one()?.token == RightBrace {
            eat!(self, Comma);
            eat!(self, RightBrace);
            return Ok(true);
        }

        Ok(false)
    }

    fn new_initializer(&mut self, mut ty: Type, is_flexible: bool) -> Result<Initializer> {
        let mut init = Initializer::new(ty.clone(), false);

        if ty.is_array() {
            if is_flexible && ty.get_size() < 0 {
                init.is_flexible = true;
                return Ok(init);
            }
            for _ in 0..=ty.get_array_len() - 1 {
                init.add_child(self.new_initializer(ty.base().unwrap(), false)?);
            }
        }

        if ty.is_struct() {
            let mut i = 0;
            let len = ty.get_members().unwrap().len();
            for mem in ty.get_members().unwrap() {
                i += 1;
                if is_flexible && ty.is_flexible() && i == len {
                    let child = Initializer::new(mem.ty, true);
                    init.add_child(child);
                } else {
                    init.add_child(self.new_initializer(mem.ty, false)?);
                }
            }

            return Ok(init);
        }

        if ty.is_union() {
            let mut i = 0;
            let len = ty.get_members().unwrap().len();
            for mem in ty.get_members().unwrap() {
                i += 1;
                if is_flexible && ty.is_flexible() && i == len {
                    let child = Initializer::new(mem.ty, true);
                    init.add_child(child);
                } else {
                    init.add_child(self.new_initializer(mem.ty, false)?);
                }
            }

            return Ok(init);
        }

        Ok(init)
    }

    fn skip_excess_element(&mut self) -> Result<()> {
        if self.peek()?.token == LeftBrace {
            eat!(self, LeftBrace);
            self.skip_excess_element()?;
            eat!(self, RightBrace);
            return Ok(());
        }
        self.assign()?;
        Ok(())
    }

    /// string-initializer = string-literal
    fn string_initializer(&mut self, init: &mut Initializer) -> Result<()> {
        let mut s;
        let pos = eat!(self, Str, s);
        s.push('\0');

        if init.is_flexible {
            *init = self.new_initializer(
                Type::array_type(init.ty.base().unwrap(), s.len() as i32),
                false,
            )?;
        }

        let len = if init.ty.get_array_len() as usize > s.len() {
            s.len()
        } else {
            init.ty.get_array_len() as usize
        };

        for i in 0..len {
            let num = s.as_bytes().get(i).unwrap();
            init.get_child(i as i32).expr = Some(ExprWithPos::new_number(*num as i64, pos));
        }

        Ok(())
    }

    fn count_array_init_elements(&mut self, ty: Type) -> Result<i32> {
        let mut dummy = self.new_initializer(ty.base().unwrap(), false)?;
        let mut i = 0;
        while !self.consume_end()? {
            if i > 0 {
                eat!(self, Comma);
            }
            self.initializer2(&mut dummy)?;
            i += 1;
        }
        Ok(i)
    }

    /// array-initializer1 = "{" initializer ("," initializer)* ","? "}"
    fn array_initializer1(&mut self, init: &mut Initializer) -> Result<()> {
        eat!(self, LeftBrace);

        let old_current_pos = self.current_pos;

        if init.is_flexible {
            let len = self.count_array_init_elements(init.ty.clone())?;
            *init = self.new_initializer(Type::array_type(init.ty.base().unwrap(), len), false)?;
        }

        self.current_pos = old_current_pos;

        let mut i = 0;
        while !self.consume_end()? {
            if i > 0 {
                eat!(self, Comma);
            }

            if i < init.ty.get_array_len() {
                self.initializer2(init.get_child(i))?;
            } else {
                self.skip_excess_element()?;
            }

            i += 1;
        }

        Ok(())
    }

    /// array-initializer2 = initializer ("," initializer)*
    fn array_initializer2(&mut self, init: &mut Initializer) -> Result<()> {
        let old_current_pos = self.current_pos;

        if init.is_flexible {
            let len = self.count_array_init_elements(init.ty.clone())?;
            *init = self.new_initializer(Type::array_type(init.ty.base().unwrap(), len), false)?;
        }

        self.current_pos = old_current_pos;

        let mut i = 0;
        while i < init.ty.get_array_len() && self.peek()?.token != RightBrace {
            if i > 0 {
                eat!(self, Comma);
            }

            self.initializer2(init.get_child(i))?;

            i += 1;
        }

        Ok(())
    }

    /// struct-initializer1 = "{" initializer ("," initializer)* "}"
    fn struct_initializer1(&mut self, init: &mut Initializer) -> Result<()> {
        eat!(self, LeftBrace);

        let mut i = 0;
        while !self.consume_end()? {
            if i > 0 {
                eat!(self, Comma);
            }

            if (i as usize) < init.children.len() {
                self.initializer2(init.get_child(i))?;
                i += 1;
            } else {
                self.skip_excess_element()?;
            }
        }

        Ok(())
    }

    /// struct-initializer2 = initializer ("," initializer)*
    fn struct_initializer2(&mut self, init: &mut Initializer) -> Result<()> {
        let mut i = 0;
        while i < init.ty.get_members().unwrap().len() && !self.is_end()? {
            if i > 0 {
                eat!(self, Comma);
            }

            self.initializer2(init.get_child(i as i32))?;
            i += 1;
        }

        Ok(())
    }

    fn union_initializer(&mut self, init: &mut Initializer) -> Result<()> {
        // Unlike structs, union initializers take only one initializer,
        // and that initializes the first union member.
        if self.peek()?.token == LeftBrace {
            eat!(self, LeftBrace);
            self.initializer2(init.get_child(0))?;
            if self.peek()?.token == Comma {
                eat!(self, Comma);
            }
            eat!(self, RightBrace);
        } else {
            self.initializer2(init.get_child(0))?;
        }

        Ok(())
    }

    /// initializer = string-initializer | array-initializer
    ///             | struct-initializer | union-initializer
    ///             | assign
    fn initializer2(&mut self, init: &mut Initializer) -> Result<()> {
        if init.ty.is_array() {
            match self.peek()?.token {
                Tok::Str(_) => {
                    self.string_initializer(init)?;
                    return Ok(());
                }
                _ => {
                    if self.peek()?.token == LeftBrace {
                        self.array_initializer1(init)?;
                    } else {
                        self.array_initializer2(init)?;
                    }
                    return Ok(());
                }
            }
        }

        if init.ty.is_struct() {
            if self.peek()?.token == LeftBrace {
                self.struct_initializer1(init)?;
                return Ok(());
            }
            // A struct can be initialized with another struct. E.g.
            // `struct T x = y;` where y is a variable of type `struct T`.
            // Handle that case first.
            let old_pos = self.current_pos;
            let mut expr = self.assign()?;
            add_type(&mut expr);
            if expr.node.ty.is_struct() {
                init.expr = Some(expr);
                return Ok(());
            }
            self.current_pos = old_pos;
            self.struct_initializer2(init)?;
            return Ok(());
        }

        if init.ty.is_union() {
            self.union_initializer(init)?;
            return Ok(());
        }

        if self.peek()?.token == LeftBrace {
            // An initializer for a scalar variable can be surrounded by
            // braces. E.g. `int x = {3};`. Handle that case.
            eat!(self, LeftBrace);
            self.initializer2(init)?;
            eat!(self, RightBrace);
            return Ok(());
        }

        init.expr = Some(self.assign()?);

        Ok(())
    }

    fn initializer(&mut self, ty: &mut Type) -> Result<Initializer> {
        let mut init = self.new_initializer(ty.clone(), true)?;
        self.initializer2(&mut init)?;

        if (ty.is_struct() || ty.is_union()) && ty.is_flexible() {
            let _ty = &mut ty.clone();
            let len = _ty.get_members().unwrap().len();
            _ty.set_last_member_type(init.get_child(len as i32 - 1).ty.clone());
            _ty.set_size(_ty.get_size() + init.get_child(len as i32 - 1).ty.clone().get_size());

            *ty = _ty.clone();
            return Ok(init);
        }

        *ty = init.ty.clone();
        Ok(init)
    }

    fn init_desg_expr(&mut self, desg: &mut InitDesg, tok: Token) -> Result<ExprWithPos> {
        if let Some(var) = &desg.var {
            return Ok(ExprWithPos::new_var(var.clone(), tok.pos.clone()));
        } else if let Some(mem) = desg.member.clone() {
            let node = ExprWithPos::new_member_expr(
                self.init_desg_expr(desg.next(), tok.clone())?,
                mem,
                tok.pos.clone(),
            );
            return Ok(node);
        } else {
            let lhs = self.init_desg_expr(desg.next(), tok.clone())?;
            let rhs = ExprWithPos::new_number(desg.idx, tok.pos);
            return Ok(ExprWithPos::new_deref(
                self.new_add(lhs, rhs, tok.clone().pos)?,
                tok.clone().pos,
            ));
        }
    }

    fn create_local_var_init(
        &mut self,
        init: &mut Initializer,
        mut ty: Type,
        desg: &mut InitDesg,
        tok: Token,
    ) -> Result<ExprWithPos> {
        if ty.is_array() {
            let mut node = ExprWithPos::new_null_expr(tok.pos.clone());
            for i in 0..ty.get_array_len() {
                let mut desg2 = InitDesg {
                    idx: i as i64,
                    next: Some(Box::new(desg.clone())),
                    var: None,
                    member: None,
                };
                let rhs = self.create_local_var_init(
                    init.get_child(i),
                    ty.base().unwrap(),
                    &mut desg2,
                    tok.clone(),
                )?;
                node = ExprWithPos::new_comma(node, rhs, tok.pos.clone());
            }

            return Ok(node);
        }

        if ty.is_struct() && init.expr.is_none() {
            let mut node = ExprWithPos::new_null_expr(self.peek()?.pos);
            let mut i = 0;
            for mem in ty.get_members().unwrap() {
                let mut desg2 = InitDesg {
                    next: Some(Box::new(desg.clone())),
                    idx: 0,
                    member: Some(mem.clone()),
                    var: None,
                };
                let rhs =
                    self.create_local_var_init(init.get_child(i), mem.ty, &mut desg2, tok.clone())?;
                node = ExprWithPos::new_comma(node, rhs, tok.pos.clone());
                i += 1;
            }

            return Ok(node);
        }

        if ty.is_union() {
            let member = ty.get_members().unwrap().get(0).unwrap().clone();
            let mut desg2 = InitDesg {
                next: Some(Box::new(desg.clone())),
                idx: 0,
                member: Some(member.clone()),
                var: None,
            };
            return self.create_local_var_init(init.get_child(0), member.ty, &mut desg2, tok);
        }

        if init.expr.is_none() {
            return Ok(ExprWithPos::new_null_expr(self.peek()?.pos.clone()));
        }

        let mut lhs = self.init_desg_expr(desg, tok.clone())?;
        add_type(&mut lhs);
        let mut rhs = init.expr.clone().unwrap();
        add_type(&mut rhs);
        let mut node = ExprWithPos::new_assign(lhs, rhs, tok.pos);
        add_type(&mut node);
        Ok(node)
    }

    /// A variable definition with an initializer is a shorthand notation
    /// for a variable definition followed by assignments. This function
    /// generates assignment expressions for an initializer. For example,
    /// `int x[2][2] = {{6, 7}, {8, 9}}` is converted to the following
    /// expressions:
    ///
    ///   x[0][0] = 6;
    ///   x[0][1] = 7;
    ///   x[1][0] = 8;
    ///   x[1][1] = 9;
    fn local_var_initializer(&mut self, var: Rc<RefCell<Obj>>) -> Result<ExprWithPos> {
        let mut init = self.initializer(&mut var.borrow_mut().ty)?;
        let mut desg = InitDesg {
            var: Some(var.clone()),
            idx: 0,
            next: None,
            member: None,
        };
        let tok = self.peek()?.clone();
        // If a partial initializer list is given, the standard requires
        // that unspecified elements are set to 0. Here, we simply
        // zero-initialize the entire memory region of a variable before
        // initializing it with user-supplied values.
        let lhs = ExprWithPos::new_memzero(var.clone(), tok.pos.clone());
        let rhs =
            self.create_local_var_init(&mut init, var.borrow().ty.clone(), &mut desg, tok.clone())?;
        Ok(ExprWithPos::new_comma(lhs, rhs, tok.pos.clone()))
    }

    /// Initializers for global variables are evaluated at compile-time and
    /// embedded to .data section. This function serializes Initializer
    /// objects to a flat byte array. It is a compile error if an
    /// initializer list contains a non-constant expression.
    fn global_var_initializer(&mut self, var: Rc<RefCell<Obj>>) -> Result<()> {
        let mut init = self.initializer(&mut var.borrow_mut().ty)?;
        let mut buf: Vec<u8> = vec![0; var.borrow().ty.get_size() as usize];
        let rels =
            self.write_global_var_data(&mut init, &mut var.borrow_mut().ty.clone(), &mut buf, 0)?;

        var.borrow_mut().init_data = Some(ast::InitData::BytesInitData(buf));
        var.borrow_mut().rel = rels;

        Ok(())
    }

    fn write_global_var_data(
        &mut self,
        init: &mut Initializer,
        ty: &mut Type,
        buf: &mut Vec<u8>,
        offset: i32,
    ) -> Result<Vec<Relocation>> {
        if ty.is_array() {
            let sz = ty.base().unwrap().get_size();
            let mut rels = vec![];
            for i in 0..ty.get_array_len() {
                let mut rel = self.write_global_var_data(
                    init.get_child(i),
                    &mut ty.base().unwrap().clone(),
                    buf,
                    offset + sz * i,
                )?;
                rels.append(&mut rel);
            }
            return Ok(rels);
        }

        if ty.is_struct() {
            let mut i = 0;
            let mut rels = vec![];
            for mem in &mut ty.get_members().unwrap() {
                let mut rel = self.write_global_var_data(
                    init.get_child(i),
                    &mut mem.ty,
                    buf,
                    offset + mem.offset,
                )?;
                rels.append(&mut rel);
                i += 1;
            }
            return Ok(rels);
        }

        if ty.is_union() {
            return self.write_global_var_data(
                init.get_child(0),
                &mut ty.get_members().unwrap().get(0).unwrap().ty.clone(),
                buf,
                offset,
            );
        }

        if let Some(init_expr) = &mut init.expr {
            let mut label = None;
            let val = self.eval_constexpr_with_label(init_expr, &mut label)?;
            if let Some(label) = label {
                let rel = Relocation {
                    offset,
                    label,
                    addend: val,
                };
                return Ok(vec![rel]);
            } else {
                self.write_buf(buf, offset, val, ty.get_size());
                return Ok(vec![]);
            }
        } else {
            return Ok(vec![]);
        }
    }

    fn write_buf(&mut self, buf: &mut Vec<u8>, offset: i32, val: i64, sz: i32) {
        match sz {
            1 => buf[offset as usize] = val as u8,
            2 => {
                let val = val as u16;
                buf[offset as usize] = val as u8;
                buf[(offset + 1) as usize] = (val >> 8) as u8;
            }
            4 => {
                let val = val as u32;
                buf[offset as usize] = val as u8;
                buf[(offset + 1) as usize] = (val >> 8) as u8;
                buf[(offset + 2) as usize] = (val >> 16) as u8;
                buf[(offset + 3) as usize] = (val >> 24) as u8;
            }
            8 => {
                let val = val as u64;
                buf[offset as usize] = val as u8;
                buf[(offset + 1) as usize] = (val >> 8) as u8;
                buf[(offset + 2) as usize] = (val >> 16) as u8;
                buf[(offset + 3) as usize] = (val >> 24) as u8;
                buf[(offset + 4) as usize] = (val >> 32) as u8;
                buf[(offset + 5) as usize] = (val >> 40) as u8;
                buf[(offset + 6) as usize] = (val >> 48) as u8;
                buf[(offset + 7) as usize] = (val >> 56) as u8;
            }
            _ => unreachable!(),
        }
    }

    /// stmt = "return" expr ";"
    ///      | "if" "(" expr ")" stmt ("else" stmt)?
    ///      | "switch" "(" expr ")" stmt
    ///      | "case" const-expr ":" stmt
    ///      | "default" ":" stmt
    ///      | "for" "(" expr-stmt expr? ";" expr? ")" stmt
    ///      | "while" "(" expr ")" stmt
    ///      | "goto" ident ";"
    ///      | "break" ";"
    ///      | "continue" ";"
    ///      | ident ":" stmt
    ///      | "{" compound-stmt
    ///      | expr-stmt
    fn stmt(&mut self) -> Result<StmtWithPos> {
        match self.peek()?.token.clone() {
            Tok::KeywordReturn => {
                let pos = eat!(self, KeywordReturn);
                let mut expr = self.expr()?;
                add_type(&mut expr);
                let return_ty = self.current_fn.as_mut().unwrap().get_return_ty().unwrap();
                expr = ExprWithPos::new_cast_expr(expr, return_ty, pos);
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

                self.begin_scope();

                let old_break_label = self.break_label.clone();
                self.break_label = Some(self.new_unique_name());

                let old_continue_label = self.continue_label.clone();
                self.continue_label = Some(self.new_unique_name());

                let tok = self.peek()?.token.clone();
                let init = if self.is_typename(tok)? {
                    let basety = self.declspec(&mut None)?;
                    self.declaration(basety, &None)?
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

                self.end_scope();

                let node = WithPos::new(
                    Stmt::ForStmt {
                        init: Box::new(init),
                        condition: cond,
                        body: Box::new(body),
                        increment: inc,
                        break_label: self.break_label.clone(),
                        continue_label: self.continue_label.clone(),
                    },
                    pos,
                );

                self.break_label = old_break_label;
                self.continue_label = old_continue_label;

                Ok(node)
            }
            Tok::KeywordWhile => {
                let pos = eat!(self, KeywordWhile);
                eat!(self, LeftParen);
                let cond = self.expr()?;
                eat!(self, RightParen);
                let old_break_label = self.break_label.clone();
                self.break_label = Some(self.new_unique_name());
                let old_continue_label = self.continue_label.clone();
                self.continue_label = Some(self.new_unique_name());
                let body = self.stmt()?;
                let node = WithPos::new(
                    Stmt::WhileStmt {
                        condition: cond,
                        body: Box::new(body),
                        break_label: self.break_label.clone(),
                        continue_label: self.continue_label.clone(),
                    },
                    pos,
                );
                self.break_label = old_break_label;
                self.continue_label = old_continue_label;
                Ok(node)
            }
            Tok::KeywordGoto => {
                let pos = eat!(self, KeywordGoto);
                let label;
                eat!(self, Ident, label);
                eat!(self, Semicolon);
                let node = WithPos::new(
                    Stmt::GotoStmt {
                        label: label.clone(),
                    },
                    pos,
                );
                Ok(node)
            }
            Tok::Ident(..) if self.peek_next_one()?.token == Colon => {
                let label;
                let pos = eat!(self, Ident, label);
                eat!(self, Colon);
                let stmt = self.stmt()?;
                let unique_name = self.new_unique_name();

                self.goto_labels.insert(label.clone(), unique_name);

                let node = WithPos::new(
                    Stmt::LabelStmt {
                        label: label.clone(),
                        stmt: Box::new(stmt),
                    },
                    pos,
                );
                Ok(node)
            }
            Tok::KeywordBreak => {
                let pos = eat!(self, KeywordBreak);

                if let Some(brk_label) = self.break_label.clone() {
                    eat!(self, Semicolon);
                    self.goto_labels
                        .insert(brk_label.clone(), brk_label.clone());

                    let node = WithPos::new(
                        Stmt::GotoStmt {
                            label: brk_label.clone(),
                        },
                        pos,
                    );
                    Ok(node)
                } else {
                    panic!("stray break: {}", pos);
                }
            }
            Tok::KeywordContinue => {
                let pos = eat!(self, KeywordContinue);

                if let Some(cont_label) = self.continue_label.clone() {
                    eat!(self, Semicolon);
                    self.goto_labels
                        .insert(cont_label.clone(), cont_label.clone());

                    let node = WithPos::new(
                        Stmt::GotoStmt {
                            label: cont_label.clone(),
                        },
                        pos,
                    );
                    Ok(node)
                } else {
                    panic!("stray continue: {}", pos);
                }
            }
            Tok::KeywordSwitch => {
                let pos = eat!(self, KeywordSwitch);
                eat!(self, LeftParen);
                let condition = self.expr()?;
                let mut node = StmtWithPos::new_switch(condition, pos);
                eat!(self, RightParen);

                let old_case_stmts = self.case_stmts.clone();
                let old_default_case_stmt = self.default_case_stmt.clone();
                let old_break_label = self.break_label.clone();

                self.break_label = Some(self.new_unique_name());
                node.update_break_label(self.break_label.clone());

                let body = self.stmt()?;
                node.update_body(body);

                node.update_cases(self.case_stmts.clone());
                node.update_default_case(self.default_case_stmt.clone());

                self.case_stmts = old_case_stmts;
                self.default_case_stmt = old_default_case_stmt;
                self.break_label = old_break_label;

                return Ok(node);
            }
            Tok::KeywordCase => {
                let pos = eat!(self, KeywordCase);
                let val = self.const_expr()?;
                eat!(self, Colon);

                let label = self.new_unique_name();
                let stmt = self.stmt()?;
                let val = val;
                let node = WithPos::new(
                    Stmt::CaseStmt {
                        label,
                        val,
                        stmt: Box::new(stmt),
                    },
                    pos,
                );

                self.case_stmts.insert(0, node.clone());

                return Ok(node);
            }
            Tok::KeywordDefault => {
                let pos = eat!(self, KeywordDefault);
                eat!(self, Colon);

                let label = self.new_unique_name();
                let stmt = self.stmt()?;
                let node = WithPos::new(
                    Stmt::CaseStmt {
                        label,
                        val: 0,
                        stmt: Box::new(stmt),
                    },
                    pos,
                );
                self.default_case_stmt = Some(node.clone());
                return Ok(node);
            }
            _ => self.expr_stmt(),
        }
    }

    /// declaration = declspec (declarator ("=" expr)? ("," declarator ("=" expr)?)*)? ";"
    fn declaration(&mut self, basety: Type, attr: &Option<VarAttr>) -> Result<StmtWithPos> {
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
                    if ty.is_void() {
                        panic!("{:#?} variable declared void.", ty);
                    }
                    if let Some(attr) = attr {
                        if attr.is_static() {
                            let unique_name = self.new_unique_name();
                            let var = self.new_global_variable(
                                unique_name,
                                ty.clone(),
                                Some(InitData::IntInitData(0)),
                            )?;
                            if self.peek()?.token == Equal {
                                eat!(self, Equal);
                                self.global_var_initializer(var.clone())?;
                            }

                            self.var_env
                                .enter(self.symbols.symbol(&ty.clone().get_ident().unwrap()), var);
                            continue;
                        }
                    }
                    let ident = ty.get_ident().unwrap();
                    let var = self.new_local_variable(ident.clone(), ty.clone())?;
                    if let Some(attr) = attr {
                        if attr.align > 0 {
                            var.borrow_mut().align = attr.align;
                        }
                    }

                    if self.peek()?.token == Equal {
                        let pos = eat!(self, Equal);
                        let expr = self.local_var_initializer(var.clone())?;
                        let expr_stmt = WithPos::new(Stmt::ExprStmt { expr }, pos);
                        decls.push(expr_stmt);
                    }

                    if self.typedef_env.look(self.symbols.symbol(&ident)).is_none()
                        && var.borrow().ty.get_size() < 0
                    {
                        panic!("variable {} has incomplete type", var.borrow());
                    }
                    if var.borrow().ty.is_void() {
                        panic!("variable {} declared void", var.borrow());
                    }
                }
            }
        }

        Ok(WithPos::new(Stmt::Block { body: decls }, self.peek()?.pos))
    }

    // struct-members = (declspec declarator (","  declarator)* ";")*
    fn struct_members(&mut self, ty: &mut Type) -> Result<()> {
        // Construct a struct object.
        let mut members = vec![];
        loop {
            match self.peek()?.token {
                Tok::RightBrace => {
                    eat!(self, RightBrace);
                    break;
                }
                _ => {
                    let mut attr = Some(VarAttr::new_placeholder());
                    let basety = self.declspec(&mut attr)?;
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
                                let member_name = member_ty.get_token().unwrap();
                                let member_align = if let Some(ref attr) = attr {
                                    if attr.align > 0 {
                                        attr.align
                                    } else {
                                        member_ty.get_align()
                                    }
                                } else {
                                    member_ty.get_align()
                                };
                                let member = sema::Member {
                                    ty: member_ty,
                                    name: member_name,
                                    offset: 0,
                                    align: member_align,
                                };
                                members.push(member);
                            }
                        }
                    }
                }
            }
        }

        if members.last().is_some()
            && members.last().unwrap().ty.is_array()
            && members.last().unwrap().ty.get_array_len() < 0
        {
            let mut m = members.last().unwrap().clone();
            m.ty.set_array_len(0);
            members.remove(members.len() - 1);
            members.push(m);
            ty.set_flexible(true);
        }

        ty.set_members(members);
        Ok(())
    }

    // struct-union-decl = ident? ("{" struct-members)?
    fn struct_union_decl(&mut self) -> Result<Type> {
        // Read a struct tag.
        let (tag, tag_token) = match self.peek()?.token {
            Tok::Ident(..) => {
                let ident;
                let tok = self.peek()?.clone();
                eat!(self, Ident, ident);
                (Some(ident), Some(tok))
            }
            _ => (None, None),
        };

        match self.peek()?.token {
            Tok::LeftBrace => {
                eat!(self, LeftBrace);
            }
            _ => {
                if let Some(struct_tag) = tag {
                    let ty = self
                        .struct_union_tag_env
                        .look(self.symbols.symbol(&struct_tag));
                    if let Some(_ty) = ty {
                        return Ok(_ty.clone());
                    } else {
                        let ty = Type::struct_type(tag_token, vec![], -1, 1);
                        self.struct_union_tag_env
                            .enter(self.symbols.symbol(&struct_tag), ty.clone());
                        return Ok(ty);
                    }
                }
            }
        }

        let mut ty = Type::struct_type(tag_token, vec![], 0, 1);
        self.struct_members(&mut ty)?;

        if let Some(tag) = tag {
            // Assign offsets within the struct to members.
            let mut offset = 0;
            let mut members = ty.get_members().unwrap();
            for mem in &mut members {
                offset = align_to(offset, mem.ty.get_align());
                mem.offset = offset;
                offset += mem.ty.get_size();

                if ty.get_align() < mem.ty.get_align() {
                    ty.set_align(mem.ty.get_align());
                }
            }

            ty.set_size(sema::align_to(offset, ty.get_align()));
            ty.set_members(members);

            self.struct_union_tag_env
                .enter(self.symbols.symbol(&tag), ty.clone());
        }

        return Ok(ty);
    }

    /// struct-decl = "{" struct-members
    /// struct-members = (declspec declarator (","  declarator)* ";")*
    fn struct_decl(&mut self) -> Result<Type> {
        let mut ty = self.struct_union_decl()?;
        if ty.get_size() < -1 {
            return Ok(ty);
        }

        // Assign offsets within the struct to members.
        let mut offset = 0;
        let mut members = ty.get_members().unwrap();
        for mem in &mut members {
            offset = align_to(offset, mem.align);
            mem.offset = offset;
            offset += mem.ty.get_size();

            if ty.get_align() < mem.align {
                ty.set_align(mem.align);
            }
        }

        ty.set_size(sema::align_to(offset, ty.get_align()));
        ty.set_members(members);

        Ok(ty)
    }

    /// union-decl = "{" struct-members
    /// union-members = (declspec declarator (","  declarator)* ";")*
    fn union_decl(&mut self) -> Result<Type> {
        let mut ty = self.struct_union_decl()?;
        if ty.get_size() < -1 {
            return Ok(ty);
        }

        // If union, we don't have to assign offsets because they
        // are already initialized to zero. We need to compute the
        // alignment and the size though.
        let mut members = ty.get_members().unwrap();
        for mem in &mut members {
            if ty.get_align() < mem.align {
                ty.set_align(mem.align);
            }

            if ty.get_size() < mem.ty.get_size() {
                ty.set_size(mem.ty.get_size());
            }
        }

        let ty = Type::union_type(
            ty.name.clone(),
            members,
            sema::align_to(ty.get_size(), ty.get_align()),
            ty.get_align(),
        );

        Ok(ty)
    }

    /// declspec = ("void" | "_Bool" | "char" | "short" | "int" | "long"
    ///          | "typedef" | "static" | "extern"
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
        let mut ty = Type::new_int();

        let mut typedef_static_extern_count = 0;

        loop {
            let tok = self.peek()?.token.clone();
            if !self.is_typename(tok)? {
                break;
            }
            match self.peek()?.token.clone() {
                Tok::KeywordTypedef => {
                    eat!(self, KeywordTypedef);
                    if attr.is_some() {
                        typedef_static_extern_count += 1;
                        if typedef_static_extern_count > 1 {
                            panic!("typedef may not be used together with static or extern.");
                        }
                        *attr = Some(VarAttr::new_typedef());
                        continue;
                    } else {
                        panic!("storage class specifier is not allowed in this context.");
                    }
                }
                Tok::KeywordStatic => {
                    eat!(self, KeywordStatic);
                    if attr.is_some() {
                        typedef_static_extern_count += 1;
                        if typedef_static_extern_count > 1 {
                            panic!("typedef may not be used together with static or extern.");
                        }
                        *attr = Some(VarAttr::new_static());
                        continue;
                    } else {
                        panic!("storage class specifier is not allowed in this context.");
                    }
                }
                Tok::KeywordExtern => {
                    eat!(self, KeywordExtern);
                    if attr.is_some() {
                        typedef_static_extern_count += 1;
                        if typedef_static_extern_count > 1 {
                            panic!("typedef may not be used together with static or extern.");
                        }
                        *attr = Some(VarAttr::new_extern());
                        continue;
                    } else {
                        panic!("storage class specifier is not allowed in this context.");
                    }
                }
                Tok::KeywordAlignas => {
                    eat!(self, KeywordAlignas);
                    if let Some(attr) = attr {
                        eat!(self, LeftParen);
                        let current_tok = self.peek()?.token.clone();
                        if self.is_typename(current_tok)? {
                            attr.align = self.typename()?.get_align();
                        } else {
                            attr.align = self.const_expr()? as i32;
                        }
                        eat!(self, RightParen);
                        continue;
                    } else {
                        panic!("_Alignas is not allowed in this context");
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
                    self.token()?;
                    if let Some(_ty) = self.typedef_env.look(self.symbols.symbol(&name)) {
                        ty = _ty.clone();
                        counter += Counter::OTHER as i32;
                        continue;
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
                ty = Type::new_void();
            } else if counter == Counter::BOOL as i32 {
                ty = Type::new_bool();
            } else if counter == Counter::CHAR as i32 {
                ty = Type::new_char();
            } else if counter == Counter::SHORT as i32 {
                ty = Type::new_short();
            } else if counter == Counter::SHORT as i32 + Counter::INT as i32 {
                ty = Type::new_short();
            } else if counter == Counter::INT as i32 {
                ty = Type::new_int();
            } else if counter == Counter::LONG as i32 + Counter::LONG as i32 + Counter::INT as i32 {
                ty = Type::new_long();
            } else if counter == Counter::LONG as i32 + Counter::INT as i32 {
                ty = Type::new_long();
            } else if counter == Counter::LONG as i32 + Counter::LONG as i32 {
                ty = Type::new_long();
            } else if counter == Counter::LONG as i32 {
                ty = Type::new_long();
            } else {
                panic!("invalid type.");
            }
        }

        Ok(ty)
    }

    /// func-params = ("void" | param ("," param)*)? ")"
    /// param       = declspec declarator
    fn func_params(&mut self, ty: Type) -> Result<Type> {
        let mut params = vec![];
        if self.peek()?.token == KeywordVoid && self.peek_next_one()?.token == RightParen {
            eat!(self, KeywordVoid);
            eat!(self, RightParen);
            return Ok(Type::new_func(params, ty));
        }
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
                    if ty2.is_array() {
                        let type_name = ty2.clone().name.unwrap();
                        ty2 = pointer_to(ty2.base().unwrap());
                        ty2.set_name(type_name);
                    }

                    params.push(ty2);
                }
            }
        }

        let node = Type::new_func(params, ty);
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

        while self.peek()?.token == Star {
            eat!(self, Star);
            ty = sema::pointer_to(ty);
        }

        if self.peek()?.token == LeftParen
            && self.peek_next_one()?.token.is_ident()
            && self.peek_next_two()?.token == RightParen
        {
            eat!(self, LeftParen);
            let tok = self.token()?;
            eat!(self, RightParen);
            let mut ty = self.type_suffix(ty)?;
            ty.set_name(tok);
            return Ok(ty);
        }

        if self.peek()?.token == LeftParen {
            eat!(self, LeftParen);
            let mut _ty = self.declarator(ty.clone())?;
            eat!(self, RightParen);
            let ty = self.type_suffix(ty)?;
            match _ty.ty {
                Ty::TyPtr { ref mut base, .. } | Ty::TyArray { ref mut base, .. } => {
                    *base = Box::new(ty);
                }
                _ => panic!(),
            }
            return Ok(_ty);
        }

        if self.peek()?.token.is_ident() {
            let tok = self.token()?;
            ty = self.type_suffix(ty)?;
            ty.set_name(tok);
            return Ok(ty);
        }

        panic!("expected a variable name, but got {:?}", self.peek()?)
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
            | Tok::KeywordExtern
            | Tok::KeywordAlignas
            | Tok::KeywordVoid => Ok(true),
            Tok::Ident(name) => {
                let symbol = self.symbols.symbol(&name);
                Ok(self.typedef_env.look(symbol).is_some())
            }
            _ => Ok(false),
        }
    }

    /// compound-stmt = (declaration | stmt)* "}"
    fn compound_stmt(&mut self) -> Result<StmtWithPos> {
        let mut stmts = vec![];

        self.begin_scope();
        loop {
            match self.peek()?.token {
                Tok::RightBrace => {
                    eat!(self, RightBrace);
                    break;
                }
                _ => {
                    // { typedef int t; t t=1; t t=2; t; }
                    let tok = self.peek()?.token.clone();
                    match tok.clone() {
                        Tok::Ident(name) => {
                            let sym = self.symbols.symbol(&name);
                            // handle case like `t t=2`;
                            if self.var_env.look(sym).is_some()
                                && !self.peek_next_one()?.token.is_ident()
                            {
                                let mut stmt = self.stmt()?;
                                sema_stmt(&mut stmt);
                                stmts.push(stmt);
                                continue;
                            }
                        }
                        _ => (),
                    }
                    if self.is_typename(tok)? && self.peek_next_one()?.token != Colon {
                        let mut attr = Some(VarAttr::new_placeholder());
                        let basety = self.declspec(&mut attr)?;
                        if let Some(attr) = attr.clone() {
                            if attr.is_typedef() {
                                self.parse_typedef(basety)?;
                                continue;
                            }
                        }

                        let pos = self.current_pos;
                        if self.is_function_definition()? {
                            self.current_pos = pos;
                            self.function(basety, &attr)?;
                            continue;
                        } else {
                            self.current_pos = pos;
                        }
                        if attr.clone().unwrap().is_extern() {
                            self.global_variable(basety, &attr)?;
                            continue;
                        }
                        let mut declaration = self.declaration(basety, &attr)?;
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
        self.end_scope();
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
                        Type::new_placeholder(),
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
                // tmp
                let mut l_value = ExprWithPos::new_var(var.clone(), left.pos.clone());
                add_type(&mut l_value);
                let mut r_value = ExprWithPos::new_addr(*left.clone(), left.pos.clone());
                add_type(&mut r_value);
                let mut expr1 = ExprWithPos::new_assign(l_value.clone(), r_value, left.pos.clone());
                add_type(&mut expr1);

                // *tmp = *tmp op B
                let mut l_value = ExprWithPos::new_deref(l_value, left.pos);
                add_type(&mut l_value);
                let mut r_value =
                    ExprWithPos::new_binary(op.node, l_value.clone(), *right, left.pos);
                add_type(&mut r_value);
                let mut expr2 = ExprWithPos::new_assign(l_value, r_value, left.pos);
                add_type(&mut expr2);

                let mut node = ExprWithPos::new_comma(expr1, expr2, left.pos);
                add_type(&mut node);

                Ok(node)
            }
            _ => panic!(),
        }
    }

    /// assign    = conditional (assign-op assign)?
    /// assign-op = "=" | "+=" | "-=" | "*=" | "/=" | "%=" | "&=" | "|=" | "^="
    ///           | "<<=" | ">>="
    fn assign(&mut self) -> Result<ExprWithPos> {
        let mut expr = self.conditional()?;
        add_type(&mut expr);
        match self.peek()?.token {
            Tok::Equal => {
                let pos = eat!(self, Equal);
                let r_value = self.assign()?;
                expr = ExprWithPos::new_assign(expr, r_value, pos);
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
                expr = ExprWithPos::new_binary(ast::BinaryOperator::Mul, expr, rhs, pos);
                expr = self.to_assign(expr)?;
            }
            Tok::SlashEqual => {
                let pos = eat!(self, SlashEqual);
                let rhs = self.assign()?;
                expr = ExprWithPos::new_binary(ast::BinaryOperator::Div, expr, rhs, pos);
                expr = self.to_assign(expr)?;
            }
            Tok::PercentEqual => {
                let pos = eat!(self, PercentEqual);
                let rhs = self.assign()?;
                expr = ExprWithPos::new_binary(ast::BinaryOperator::Mod, expr, rhs, pos);
                expr = self.to_assign(expr)?;
            }
            Tok::AmpEqual => {
                let pos = eat!(self, AmpEqual);
                let rhs = self.assign()?;
                expr = ExprWithPos::new_binary(ast::BinaryOperator::BitAnd, expr, rhs, pos);
                expr = self.to_assign(expr)?;
            }
            Tok::BarEqual => {
                let pos = eat!(self, BarEqual);
                let rhs = self.assign()?;
                expr = ExprWithPos::new_binary(ast::BinaryOperator::BitOr, expr, rhs, pos);
                expr = self.to_assign(expr)?;
            }
            Tok::HatEqual => {
                let pos = eat!(self, HatEqual);
                let rhs = self.assign()?;
                expr = ExprWithPos::new_binary(ast::BinaryOperator::BitXor, expr, rhs, pos);
                expr = self.to_assign(expr)?;
            }
            Tok::LesserLesserEqual => {
                let pos = eat!(self, LesserLesserEqual);
                let rhs = self.assign()?;
                expr = ExprWithPos::new_binary(ast::BinaryOperator::SHL, expr, rhs, pos);
                expr = self.to_assign(expr)?;
            }
            Tok::GreaterGreaterEqual => {
                let pos = eat!(self, GreaterGreaterEqual);
                let rhs = self.assign()?;
                expr = ExprWithPos::new_binary(ast::BinaryOperator::SHR, expr, rhs, pos);
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
            match self.peek()?.token {
                Tok::EqualEqual => {
                    let pos = eat!(self, EqualEqual);
                    let right = self.relational()?;
                    expr = ExprWithPos::new_binary(ast::BinaryOperator::Eq, expr, right, pos);
                }
                Tok::BangEqual => {
                    let pos = eat!(self, BangEqual);
                    let right = self.relational()?;
                    expr = ExprWithPos::new_binary(ast::BinaryOperator::Ne, expr, right, pos);
                }
                _ => break,
            }
        }
        Ok(expr)
    }

    /// relational = shift ("<" shift | "<=" shift | ">" shift | ">=" shift)*
    fn relational(&mut self) -> Result<ExprWithPos> {
        let mut expr = self.shift()?;

        loop {
            match self.peek()?.token {
                Tok::Lesser => {
                    let pos = eat!(self, Lesser);
                    let right = self.shift()?;
                    expr = ExprWithPos::new_binary(ast::BinaryOperator::Lt, expr, right, pos);
                }
                Tok::LesserEqual => {
                    let pos = eat!(self, LesserEqual);
                    let right = self.shift()?;
                    expr = ExprWithPos::new_binary(ast::BinaryOperator::Le, expr, right, pos);
                }
                Tok::Greater => {
                    let pos = eat!(self, Greater);
                    let right = self.shift()?;
                    expr = ExprWithPos::new_binary(ast::BinaryOperator::Gt, expr, right, pos);
                }
                Tok::GreaterEqual => {
                    let pos = eat!(self, GreaterEqual);
                    let right = self.shift()?;
                    expr = ExprWithPos::new_binary(ast::BinaryOperator::Ge, expr, right, pos);
                }
                _ => break,
            }
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

    /// shift = add ("<<" add | ">>" add)*
    fn shift(&mut self) -> Result<ExprWithPos> {
        let mut node = self.add()?;

        loop {
            match self.peek()?.token {
                Tok::LesserLesser => {
                    let pos = eat!(self, LesserLesser);
                    let rhs = self.add()?;
                    node = ExprWithPos::new_binary(BinaryOperator::SHL, node, rhs, pos);
                }
                Tok::GreaterGreater => {
                    let pos = eat!(self, GreaterGreater);
                    let rhs = self.add()?;
                    node = ExprWithPos::new_binary(BinaryOperator::SHR, node, rhs, pos);
                }
                _ => break,
            }
        }

        Ok(node)
    }

    /// conditional = logor ("?" expr ":" conditional)?
    fn conditional(&mut self) -> Result<ExprWithPos> {
        let condition = self.logor()?;
        match self.peek()?.token {
            Tok::QuestionMark => {
                let pos = eat!(self, QuestionMark);
                let then_clause = self.expr()?;
                eat!(self, Colon);
                let else_clause = self.conditional()?;
                let node = ExprWithPos::new_ternary(condition, then_clause, else_clause, pos);
                Ok(node)
            }
            _ => Ok(condition),
        }
    }

    /// logor = logand ("||" logand)*
    fn logor(&mut self) -> Result<ExprWithPos> {
        let mut node = self.logand()?;

        loop {
            match self.peek()?.token {
                Tok::BarBar => {
                    let pos = eat!(self, BarBar);
                    let rhs = self.logand()?;
                    node = ExprWithPos::new_binary(BinaryOperator::LogOr, node, rhs, pos);
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
                    node = ExprWithPos::new_binary(BinaryOperator::LogAnd, node, rhs, pos);
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
                    node = ExprWithPos::new_binary(BinaryOperator::BitOr, node, rhs, pos);
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
                    node = ExprWithPos::new_binary(BinaryOperator::BitXor, node, rhs, pos);
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
                    node = ExprWithPos::new_binary(BinaryOperator::BitAnd, node, rhs, pos);
                }
                _ => break,
            }
        }

        Ok(node)
    }

    /// mul = cast ("*" cast | "/" cast)*
    fn mul(&mut self) -> Result<ExprWithPos> {
        let mut node = self.cast()?;

        loop {
            match self.peek()?.token {
                Tok::Star => {
                    let pos = eat!(self, Star);
                    let rhs = self.cast()?;
                    node = ExprWithPos::new_binary(BinaryOperator::Mul, node, rhs, pos);
                }
                Tok::Slash => {
                    let pos = eat!(self, Slash);
                    let rhs = self.cast()?;
                    node = ExprWithPos::new_binary(BinaryOperator::Div, node, rhs, pos);
                }
                Tok::Percent => {
                    let pos = eat!(self, Percent);
                    let rhs = self.cast()?;
                    node = ExprWithPos::new_binary(BinaryOperator::Mod, node, rhs, pos);
                }
                _ => break,
            }
        }

        Ok(node)
    }

    /// unary = ("+" | "-" | "*" | "&" | "!" | "~") cast
    ///       | ("++" | "--") unary
    ///       | postfix
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
                        Type::new_placeholder(),
                    ),
                    pos,
                ))
            }
            Amp => {
                let pos = eat!(self, Amp);
                let mut expr = self.cast()?;
                add_type(&mut expr);
                Ok(ExprWithPos::new_addr(expr, pos))
            }
            Star => {
                let pos = eat!(self, Star);
                let mut expr = self.cast()?;
                add_type(&mut expr);
                Ok(ExprWithPos::new_deref(expr, pos))
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
                        Type::new_placeholder(),
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
                        Type::new_placeholder(),
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
                let old_pos = self.current_pos;
                let next_one_tok = self.peek_next_one()?.token.clone();
                if self.is_typename(next_one_tok)? {
                    let pos = eat!(self, LeftParen);
                    let ty = self.typename()?;
                    eat!(self, RightParen);

                    // compound literal
                    if self.peek()?.token == LeftBrace {
                        self.current_pos = old_pos;
                        return self.unary();
                    }

                    // type cast
                    let node = ExprWithPos::new_cast_expr(self.cast()?, ty, pos);
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

        match (lhs.node.ty.clone().ty, rhs.node.ty.clone().ty) {
            // num - num
            _ if lhs.node.ty.is_integer() && rhs.node.ty.is_integer() => {
                lhs = ExprWithPos::new_binary(BinaryOperator::Sub, lhs, rhs, pos);
            }
            // ptr - num
            (Ty::TyPtr { base, .. } | Ty::TyArray { base, .. }, Ty::TyInt | Ty::TyLong) => {
                // num * 8
                let num = ExprWithPos::new_number(base.get_size() as i64, pos);
                rhs = ExprWithPos::new_binary(BinaryOperator::Mul, rhs, num, pos);
                add_type(&mut rhs);
                lhs = ExprWithPos::new_binary(BinaryOperator::Sub, lhs, rhs, pos);
            }
            // ptr - ptr, which returns how many elements are between the two.
            (Ty::TyPtr { base, .. }, Ty::TyPtr { .. }) => {
                let num = ExprWithPos::new_number(base.get_size() as i64, pos);
                lhs = ExprWithPos::new_binary(BinaryOperator::Sub, lhs, rhs, pos);
                lhs = ExprWithPos::new_binary(BinaryOperator::Div, lhs, num, pos);
            }
            // other
            _ => panic!(
                "invalid operands for pointer arithmetic sub. {:?} ===== {:?}",
                lhs, rhs
            ),
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

        match (lhs.node.ty.clone().ty, rhs.node.ty.clone().ty) {
            // ptr + num
            (Ty::TyPtr { base, .. } | Ty::TyArray { base, .. }, Ty::TyInt | Ty::TyLong) => {
                // num * 8
                let num = ExprWithPos::new_number(base.get_size() as i64, pos);
                rhs = ExprWithPos::new_binary(ast::BinaryOperator::Mul, rhs, num, pos);
                lhs = ExprWithPos::new_binary_with_type(
                    ast::BinaryOperator::Add,
                    lhs.clone(),
                    rhs,
                    pos,
                    lhs.node.ty,
                );
            }
            // num + ptr
            (Ty::TyInt | Ty::TyLong, Ty::TyPtr { base, .. } | Ty::TyArray { base, .. }) => {
                // num * 8
                let num = ExprWithPos::new_number(base.get_size() as i64, pos);
                lhs = ExprWithPos::new_binary(ast::BinaryOperator::Mul, lhs, num, pos);
                lhs = ExprWithPos::new_binary_with_type(
                    ast::BinaryOperator::Add,
                    rhs.clone(),
                    lhs,
                    pos,
                    rhs.node.ty,
                );
            }
            // num + num
            _ if lhs.node.ty.is_integer() && rhs.node.ty.is_integer() => {
                lhs = ExprWithPos::new_binary(ast::BinaryOperator::Add, lhs, rhs, pos);
            }
            // other
            _ => panic!("invalid operands for pointer arithmetic add."),
        }

        Ok(lhs)
    }

    fn struct_ref(&mut self, node: ExprWithPos) -> Result<ExprWithPos> {
        match node.node.ty.clone().ty {
            Ty::TyStruct { members, .. } | Ty::TyUnion { members, .. } => {
                let mem_ident;
                let pos = eat!(self, Ident, mem_ident);
                let mut mem_found = None;
                let name = node.node.ty.get_ident();

                if name.is_some() {
                    let name = node.node.ty.get_ident().unwrap();
                    let ty = self.struct_union_tag_env.look(self.symbols.symbol(&name));

                    if let Some(ty) = ty {
                        match ty.clone().ty {
                            Ty::TyStruct { members, .. } => {
                                for mem in members {
                                    match mem.name.token.clone() {
                                        Tok::Ident(tok_name) => {
                                            if mem_ident == tok_name {
                                                mem_found = Some(mem.clone());
                                                break;
                                            }
                                        }
                                        _ => (),
                                    }
                                }

                                let mem_expr =
                                    ExprWithPos::new_member_expr(node, mem_found.unwrap(), pos);
                                return Ok(mem_expr);
                            }
                            _ => (),
                        }
                    }
                }

                for mem in members {
                    match mem.name.token.clone() {
                        Tok::Ident(tok_name) => {
                            if mem_ident == tok_name {
                                mem_found = Some(mem.clone());
                                break;
                            }
                        }
                        _ => (),
                    }
                }

                let mut mem_expr = ExprWithPos::new_member_expr(node, mem_found.unwrap(), pos);
                add_type(&mut mem_expr);
                return Ok(mem_expr);
            }
            _ => panic!("must be struct or union type."),
        }
    }

    /// Convert A++ to `(typeof A)((A += 1) - 1)`
    /// Convert A-- to `(typeof A)((A -= 1) + 1)`
    fn new_inc_dec(&mut self, mut node: ExprWithPos, addend: i32) -> Result<ExprWithPos> {
        add_type(&mut node);

        let pos = self.peek()?.pos;

        let positive_one = ExprWithPos::new_number(addend as i64, pos);
        let negative_one = ExprWithPos::new_number(-addend as i64, pos);

        // A += 1
        // A + 1
        let a_plus_one = self.new_add(node.clone(), positive_one, pos)?;
        // A = A + 1
        let a = self.to_assign(a_plus_one)?;
        let e = self.new_add(a, negative_one, pos)?;

        // cast
        let c = ExprWithPos::new_cast_expr(e, node.node.ty, pos);

        Ok(c)
    }

    /// postfix = "(" type-name ")" "{" initializer-list "}"
    ///         | primary ("[" expr "]" | "." ident | "->" ident | "++" | "--")*
    fn postfix(&mut self) -> Result<ExprWithPos> {
        let current_token = self.peek()?.token.clone();
        let next_token = self.peek_next_one()?.token.clone();
        if current_token == LeftParen && self.is_typename(next_token)? {
            // Compound literal
            let pos = eat!(self, LeftParen);
            let ty = self.typename()?;
            eat!(self, RightParen);

            if self.var_env.is_parent_empty()
                && self.typedef_env.is_parent_empty()
                && self.struct_union_tag_env.is_parent_empty()
            {
                let unique_name = self.new_unique_name();
                let var =
                    self.new_global_variable(unique_name, ty, Some(InitData::IntInitData(0)))?;
                self.global_var_initializer(var.clone())?;
                let mut node = ExprWithPos::new_var(var, pos);
                add_type(&mut node);
                return Ok(node);
            }

            let var = self.new_local_variable("".to_string(), ty)?;
            let mut lhs = self.local_var_initializer(var.clone())?;
            add_type(&mut lhs);
            let mut rhs = ExprWithPos::new_var(var, pos);
            add_type(&mut rhs);
            return Ok(ExprWithPos::new_comma(lhs, rhs, pos));
        }

        let mut node = self.primary()?;

        loop {
            match self.peek()?.token {
                Tok::LeftBracket => {
                    // x[y] is short for *(x+y)
                    let pos = eat!(self, LeftBracket);
                    let idx = self.expr()?;
                    eat!(self, RightBracket);
                    node = ExprWithPos::new_deref(self.new_add(node, idx, pos)?, pos);
                }
                Tok::Dot => {
                    eat!(self, Dot);
                    add_type(&mut node);
                    node = self.struct_ref(node)?;
                }
                Tok::MinusGreater => {
                    // x->y is short for (*x).y
                    let pos = eat!(self, MinusGreater);
                    node = ExprWithPos::new_deref(node, pos);
                    add_type(&mut node);
                    node = self.struct_ref(node)?;
                    add_type(&mut node);
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

        let params_ty = match func_found.clone().unwrap().ty.clone().ty {
            Ty::TyFunc { params, .. } => params,
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
                    if arg_exp.node.ty.is_union() || arg_exp.node.ty.is_struct() {
                        panic!("passing struct or union is not supported yet.");
                    }
                    let param_type = match params_ty.get(i).clone() {
                        Some(t) => t.clone(),
                        _ => arg_exp.node.ty.clone(),
                    };
                    let node = ExprWithPos::new_cast_expr(arg_exp, param_type, pos);
                    args.push(node);
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
                match func_found.unwrap().ty.ty {
                    Ty::TyFunc { return_ty, .. } => *return_ty,
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
                self.abstract_declarator(Type::new_placeholder())?;
                eat!(self, RightParen);
                ty = self.type_suffix(ty)?;
                Ok(Type::new_ptr(ty))
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
    ///         | "_Alignof" "(" type-name ")"
    ///         | "_Alignof" unary
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
                        Ok(WithPos::new(
                            WithType::new(node, Type::new_placeholder()),
                            pos,
                        ))
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
                let mut node = ExprWithPos::new_number(value, pos);
                add_type(&mut node);
                Ok(node)
            }
            Tok::KeywordSizeof => {
                let pos = eat!(self, KeywordSizeof);
                match self.peek()?.token {
                    Tok::LeftParen => {
                        let tok = self.peek_next_one()?.token.clone();
                        if self.is_typename(tok.clone())? {
                            eat!(self, LeftParen);
                            let ty = self.typename()?;
                            eat!(self, RightParen);
                            // 处理以下特殊情况
                            // `typedef struct T T; struct T { int x; }; sizeof(T);`
                            if let Some(type_name) = ty.get_ident() {
                                if let Some(ty) = self
                                    .struct_union_tag_env
                                    .look(self.symbols.symbol(&type_name))
                                {
                                    return Ok(ExprWithPos::new_number(ty.get_size() as i64, pos));
                                }
                            }

                            Ok(ExprWithPos::new_number(ty.get_size() as i64, pos))
                        }
                        // sizeof(x)
                        else {
                            let mut node = self.unary()?;
                            add_type(&mut node);
                            node = ExprWithPos::new_number(node.node.ty.get_size() as i64, pos);
                            Ok(node)
                        }
                    }
                    // sizeof x
                    _ => {
                        let mut node = self.unary()?;
                        add_type(&mut node);
                        node = ExprWithPos::new_number(node.node.ty.get_size() as i64, pos);
                        Ok(node)
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
                    let _ty = var.borrow_mut().ty.clone();
                    match _ty.ty {
                        Ty::TyEnum => {
                            if let Some(InitData::IntInitData(i)) = var.borrow_mut().init_data {
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
                            let mut node = WithPos::new(
                                WithType::new(Expr::Variable { obj: var.clone() }, _ty),
                                pos,
                            );
                            add_type(&mut node);
                            return Ok(node);
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
                let unique_name = self.new_unique_name();
                string.push_str("\0");
                let var_type = Type::new_array(Type::new_char(), string.len() as i32);
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
            Tok::KeywordAlignof => {
                eat!(self, KeywordAlignof);
                let next_token = self.peek_next_one()?.token.clone();
                if self.peek()?.token == LeftParen && self.is_typename(next_token)? {
                    eat!(self, LeftParen);
                    let pos = self.peek()?.pos;
                    let ty = self.typename()?;
                    eat!(self, RightParen);
                    return Ok(ExprWithPos::new_number(ty.get_align() as i64, pos));
                } else {
                    let mut node = self.unary()?;
                    add_type(&mut node);
                    return Ok(ExprWithPos::new_number(
                        node.node.ty.get_align() as i64,
                        node.pos,
                    ));
                }
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

    fn peek_next_two(&mut self) -> std::result::Result<&Token, &Error> {
        if let Some(tok) = self.tokens.get(self.current_pos + 2) {
            Ok(tok)
        } else {
            panic!()
        }
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

    fn function(&mut self, basety: Type, attr: &Option<VarAttr>) -> Result<()> {
        let ty = self.declarator(basety)?;

        let is_definition = match self.peek()?.token {
            Tok::Semicolon => {
                eat!(self, Semicolon);
                false
            }
            _ => true,
        };

        let is_static = match attr {
            Some(_attr) => _attr.is_static(),
            _ => false,
        };

        self.locals = Vec::new();

        self.begin_scope();

        let ident;
        match ty.clone().ty {
            Ty::TyFunc { params, .. } => {
                ident = ty.get_ident().unwrap();
                let pos = self.peek()?.pos.clone();
                // 为了处理fib这样的递归调用，需要先把函数声明记录下来。
                let mut fndef =
                    ast::FunctionDefinition::new(ident.clone(), is_static, ty.clone(), pos);
                self.functions.push(fndef.clone());

                self.current_fn = Some(fndef.clone());

                for p in params {
                    let param_name = p.get_ident().unwrap();
                    self.new_local_variable(param_name, p)?;
                }

                // 先将函数的参数拷贝一份，因为后面在解析函数体时会加入新的局部变量
                let params = self.locals.clone();

                if is_definition {
                    eat!(self, LeftBrace);
                    fndef.body = self.compound_stmt()?;
                    fndef.params = params;
                    fndef.is_definition = is_definition;
                    fndef.locals = self.locals.clone();
                    fndef.goto_labels = self.goto_labels.clone();
                }

                self.end_scope();

                self.functions.push(fndef);
                self.goto_labels = HashMap::new();

                Ok(())
            }
            _ => panic!("ident must be function type."),
        }
    }

    fn is_function_definition(&mut self) -> Result<bool> {
        match self.peek()?.token {
            Tok::Semicolon => {
                eat!(self, Semicolon);
                Ok(false)
            }
            _ => {
                let dummy = Type::new_placeholder();
                let ty = self.declarator(dummy)?;
                Ok(ty.is_func())
            }
        }
    }

    fn new_var(&mut self, name: String, ty: Type) -> Result<Rc<RefCell<Obj>>> {
        let var = Rc::new(RefCell::new(Obj {
            name: name.clone(),
            ty: ty.clone(),
            offset: 0,
            is_local: false,
            is_definition: false,
            init_data: None,
            rel: vec![],
            align: ty.get_align(),
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
        var.borrow_mut().is_definition = true;
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

    fn global_variable(&mut self, basety: Type, attr: &Option<VarAttr>) -> Result<()> {
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
                    let var_name = ty.get_ident().unwrap();
                    if self.peek()?.token.is_ident() {
                        continue;
                    }
                    let var = self.new_global_variable(var_name, ty, None)?;
                    if let Some(attr) = attr {
                        var.borrow_mut().is_definition = !attr.is_extern();
                        if attr.align > 0 {
                            var.borrow_mut().align = attr.align;
                        }
                    }
                    if self.peek()?.token == Equal {
                        eat!(self, Equal);
                        self.global_var_initializer(var)?;
                    }
                }
            }
        }
        Ok(())
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
                    let ident = ty.get_ident().unwrap();
                    self.typedef_env.enter(self.symbols.symbol(&ident), ty);
                }
            }
        }
        Ok(())
    }

    // enum-specifier = ident? "{" enum-list? "}"
    //                | ident ("{" enum-list? "}")?
    //
    // enum-list      = ident ("=" num)? ("," ident ("=" num)?)* ","?
    fn enum_specifier(&mut self) -> Result<Type> {
        let ty = Type::new_enum();

        let mut tag = None;
        if self.peek()?.token.is_ident() {
            tag = Some(self.peek()?.clone());
            self.token()?;
        }

        if tag.is_some() && self.peek()?.token != LeftBrace {
            let ty = self.struct_union_tag_env.look(
                self.symbols
                    .symbol(&tag.clone().unwrap().token.get_ident_name().unwrap()),
            );
            match ty {
                Some(t) => {
                    if !t.is_enum() {
                        panic!();
                    } else {
                        return Ok(t.clone());
                    }
                }
                None => panic!(),
            }
        }

        eat!(self, LeftBrace);

        let mut i = 0;
        let mut val = 0;
        while !self.consume_end()? {
            if i > 0 {
                eat!(self, Comma);
            }
            i += 1;

            let name;
            eat!(self, Ident, name);

            if self.peek()?.token == Equal {
                eat!(self, Equal);
                val = self.const_expr()?;
            }

            let enum_var = Rc::new(RefCell::new(Obj {
                name: name.clone(),
                offset: 0,
                is_local: false,
                is_definition: false,
                init_data: Some(InitData::IntInitData(val as i32)),
                ty: ty.clone(),
                rel: vec![],
                align: 0,
            }));
            val += 1;
            self.var_env.enter(self.symbols.symbol(&name), enum_var);
        }

        if tag.is_some() {
            self.struct_union_tag_env.enter(
                self.symbols
                    .symbol(&tag.clone().unwrap().token.get_ident_name().unwrap()),
                ty.clone(),
            );
        }
        Ok(ty)
    }

    /// array-dimensions = const-expr? "]" type-suffix
    fn array_dimensions(&mut self, mut ty: Type) -> Result<Type> {
        match self.peek()?.token {
            Tok::RightBracket => {
                eat!(self, RightBracket);
                ty = self.type_suffix(ty)?;
                Ok(Type::new_array(ty, -1))
            }
            _ => {
                let sz = self.const_expr()?;
                eat!(self, RightBracket);
                ty = self.type_suffix(ty)?;
                Ok(Type::new_array(ty, sz as i32))
            }
        }
    }

    fn const_expr(&mut self) -> Result<i64> {
        let mut node = self.conditional()?;
        Ok(self.eval_constexpr(&mut node)?)
    }

    fn eval_constexpr(&mut self, node: &mut ExprWithPos) -> Result<i64> {
        self.eval_constexpr_with_label(node, &mut None)
    }

    /// Evaluate a given node as a constant expression.
    ///
    /// A constant expression is either just a number or ptr+n where ptr
    /// is a pointer to a global variable and n is a postiive/negative
    /// number. The latter form is accepted only as an initialization
    /// expression for a global variable.
    fn eval_constexpr_with_label(
        &mut self,
        node: &mut ExprWithPos,
        label: &mut Option<String>,
    ) -> Result<i64> {
        add_type(node);

        match node.node.node.clone() {
            Expr::Binary {
                mut left,
                op,
                mut right,
            } => match op.node {
                ast::BinaryOperator::Add => Ok(self.eval_constexpr_with_label(&mut left, label)?
                    + self.eval_constexpr(&mut right)?),
                ast::BinaryOperator::Sub => Ok(self.eval_constexpr_with_label(&mut left, label)?
                    - self.eval_constexpr(&mut right)?),
                ast::BinaryOperator::Mul => {
                    Ok(self.eval_constexpr(&mut left)? * self.eval_constexpr(&mut right)?)
                }
                ast::BinaryOperator::Div => {
                    Ok(self.eval_constexpr(&mut left)? / self.eval_constexpr(&mut right)?)
                }
                ast::BinaryOperator::Mod => {
                    Ok(self.eval_constexpr(&mut left)? % self.eval_constexpr(&mut right)?)
                }
                ast::BinaryOperator::BitAnd => {
                    Ok(self.eval_constexpr(&mut left)? & self.eval_constexpr(&mut right)?)
                }
                ast::BinaryOperator::BitOr => {
                    Ok(self.eval_constexpr(&mut left)? | self.eval_constexpr(&mut right)?)
                }
                ast::BinaryOperator::BitXor => {
                    Ok(self.eval_constexpr(&mut left)? ^ self.eval_constexpr(&mut right)?)
                }
                ast::BinaryOperator::SHL => {
                    Ok(self.eval_constexpr(&mut left)? << self.eval_constexpr(&mut right)?)
                }
                ast::BinaryOperator::SHR => {
                    Ok(self.eval_constexpr(&mut left)? >> self.eval_constexpr(&mut right)?)
                }
                ast::BinaryOperator::Eq => Ok((self.eval_constexpr(&mut left)?
                    == self.eval_constexpr(&mut right)?)
                    as i64),
                ast::BinaryOperator::Ne => Ok((self.eval_constexpr(&mut left)?
                    != self.eval_constexpr(&mut right)?)
                    as i64),
                ast::BinaryOperator::Le => Ok((self.eval_constexpr(&mut left)?
                    <= self.eval_constexpr(&mut right)?)
                    as i64),
                ast::BinaryOperator::Lt => {
                    Ok((self.eval_constexpr(&mut left)? < self.eval_constexpr(&mut right)?) as i64)
                }
                ast::BinaryOperator::Ge => Ok((self.eval_constexpr(&mut left)?
                    >= self.eval_constexpr(&mut right)?)
                    as i64),
                ast::BinaryOperator::Gt => {
                    Ok((self.eval_constexpr(&mut left)? > self.eval_constexpr(&mut right)?) as i64)
                }
                ast::BinaryOperator::LogAnd => Ok((self.eval_constexpr(&mut left)? != 0
                    && self.eval_constexpr(&mut right)? != 0)
                    as i64),
                ast::BinaryOperator::LogOr => Ok((self.eval_constexpr(&mut left)? != 0
                    || self.eval_constexpr(&mut right)? != 0)
                    as i64),
            },
            Expr::Unary { op, mut expr } => match op.node {
                ast::UnaryOperator::Neg => Ok(-self.eval_constexpr(&mut expr)?),
                ast::UnaryOperator::Not => {
                    if self.eval_constexpr(&mut expr)? == 0 {
                        return Ok(1);
                    } else {
                        return Ok(0);
                    }
                }
                ast::UnaryOperator::BitNot => Ok(!self.eval_constexpr(&mut expr)?),
            },
            Expr::CommaExpr { mut right, .. } => {
                Ok(self.eval_constexpr_with_label(&mut right, label)?)
            }
            Expr::CastExpr { mut expr, ty } => {
                let val = self.eval_constexpr_with_label(&mut expr, label)?;
                match ty.is_integer() {
                    true => match ty.get_size() {
                        1 => Ok(val as u8 as i64),
                        2 => Ok(val as u16 as i64),
                        4 => Ok(val as u32 as i64),
                        _ => Ok(val),
                    },
                    false => Ok(val),
                }
            }
            Expr::TernaryExpr {
                mut condition,
                mut then_clause,
                mut else_clause,
            } => match self.eval_constexpr(&mut condition)? {
                0 => Ok(self.eval_constexpr_with_label(&mut else_clause, label)?),
                _ => Ok(self.eval_constexpr_with_label(&mut then_clause, label)?),
            },
            Expr::Addr { mut expr } => self.eval_rvalue(&mut expr, label),
            Expr::MemberExpr { mut strct, member } => {
                if !node.node.ty.is_array() {
                    panic!()
                }
                return Ok(self.eval_rvalue(&mut strct, label)? + member.offset as i64);
            }
            Expr::Variable { obj } => {
                if !obj.borrow().ty.is_array() && !obj.borrow().ty.is_func() {
                    panic!()
                }
                *label = Some(obj.borrow().name.clone());
                return Ok(0);
            }
            Expr::Number { value } => Ok(value),
            _ => panic!("not a compile-time constant. {:?}", node.node.clone()),
        }
    }

    fn eval_rvalue(&mut self, node: &mut ExprWithPos, label: &mut Option<String>) -> Result<i64> {
        match node.node.node.clone() {
            Expr::Variable { obj } => {
                if obj.borrow().is_local {
                    panic!("{} not a compile-time constant", obj.borrow());
                }
                *label = Some(obj.borrow().name.clone());
                return Ok(0);
            }
            Expr::Deref { mut expr } => return self.eval_constexpr_with_label(&mut expr, label),
            Expr::MemberExpr { mut strct, member } => {
                return Ok(self.eval_rvalue(&mut strct, label)? + member.offset as i64);
            }
            _ => return Err(Error::InvalidInitializer { pos: node.pos }),
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
                    let mut attr = Some(VarAttr::new_placeholder());
                    let basety = self.declspec(&mut attr)?;
                    if let Some(attr) = attr.clone() {
                        if attr.is_typedef() {
                            self.parse_typedef(basety)?;
                            continue;
                        }
                    }
                    // 保存当前的位置
                    let pos = self.current_pos;
                    if self.is_function_definition()? {
                        // 回溯
                        self.current_pos = pos;
                        self.function(basety, &attr)?;
                    } else {
                        // 回溯
                        self.current_pos = pos;
                        self.global_variable(basety, &attr)?;
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
