use std::{cell::RefCell, collections::HashMap, rc::Rc, result};

use crate::{
    ast::{
        self, BinaryOperator, Expr, ExprInner, FunctionDefinition, InitData, InitDesg, Initializer,
        Obj, Program, Relocation, Stmt, StmtWithPos, UnaryOperator, VarAttr,
    },
    environment::Scope,
    error::Error::{self, UnexpectedToken},
    position::{Pos, WithPos},
    sema::{self, add_type, align_to, pointer_to, sema_stmt, Ty, Type},
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

pub struct Parser {
    tokens: Vec<Token>,
    current_pos: usize,
    /// All local variable instances created during parsing are
    /// accumulated to this map
    locals: Vec<Rc<RefCell<Obj>>>,
    globals: Vec<Rc<RefCell<Obj>>>,
    functions: Vec<FunctionDefinition>,
    unique_name_count: i32,

    scope: Scope,

    // Points to the function object the parser is currently parsing.
    current_fn: Option<FunctionDefinition>,

    goto_labels: HashMap<String, String>,
    break_label: Option<String>,
    continue_label: Option<String>,

    case_stmts: Vec<StmtWithPos>,
    default_case_stmt: Option<StmtWithPos>,
}

impl Parser {
    pub fn new(tokens: Vec<Token>) -> Self {
        Parser {
            tokens,
            current_pos: 0,
            locals: Vec::new(),
            globals: Vec::new(),
            functions: Vec::new(),
            unique_name_count: 0,

            scope: Scope::new(),

            current_fn: None,

            goto_labels: HashMap::new(),
            break_label: None,
            continue_label: None,

            case_stmts: vec![],
            default_case_stmt: None,
        }
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
            let len = ty.get_members().len();
            for mem in ty.get_members() {
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
            let len = ty.get_members().len();
            for mem in ty.get_members() {
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
            init.get_child(i as i32).expr = Some(Expr::new_long(*num as i64, pos));
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
        while i < init.ty.get_members().len() && !self.is_end()? {
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
            if expr.ty.is_struct() {
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
            let len = ty.get_members().len();
            ty.set_last_member_type(init.get_child(len as i32 - 1).ty.clone());
            ty.set_size(ty.get_size() + init.get_child(len as i32 - 1).ty.clone().get_size());

            return Ok(init);
        }

        *ty = init.ty.clone();
        Ok(init)
    }

    fn init_desg_expr(&mut self, desg: &mut InitDesg, tok: Token) -> Result<Expr> {
        if let Some(var) = &desg.var {
            return Ok(Expr::new_var(var.clone(), tok.pos.clone()));
        } else if let Some(mem) = desg.member.clone() {
            let node = Expr::new_member_expr(
                self.init_desg_expr(desg.next(), tok.clone())?,
                mem,
                tok.pos.clone(),
            );
            return Ok(node);
        } else {
            let lhs = self.init_desg_expr(desg.next(), tok.clone())?;
            let rhs = Expr::new_long(desg.idx, tok.pos);
            return Ok(Expr::new_deref(
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
    ) -> Result<Expr> {
        if ty.is_array() {
            let mut node = Expr::new_null_expr(tok.pos.clone());
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
                node = Expr::new_comma(node, rhs, tok.pos.clone());
            }

            return Ok(node);
        }

        if ty.is_struct() && init.expr.is_none() {
            let mut node = Expr::new_null_expr(self.peek()?.pos);
            let mut i = 0;
            for mem in ty.get_members() {
                let mut desg2 = InitDesg {
                    next: Some(Box::new(desg.clone())),
                    idx: 0,
                    member: Some(mem.clone()),
                    var: None,
                };
                let rhs =
                    self.create_local_var_init(init.get_child(i), mem.ty, &mut desg2, tok.clone())?;
                node = Expr::new_comma(node, rhs, tok.pos.clone());
                i += 1;
            }

            return Ok(node);
        }

        if ty.is_union() {
            let member = ty.get_members().get(0).unwrap().clone();
            let mut desg2 = InitDesg {
                next: Some(Box::new(desg.clone())),
                idx: 0,
                member: Some(member.clone()),
                var: None,
            };
            return self.create_local_var_init(init.get_child(0), member.ty, &mut desg2, tok);
        }

        if init.expr.is_none() {
            return Ok(Expr::new_null_expr(self.peek()?.pos.clone()));
        }

        let mut lhs = self.init_desg_expr(desg, tok.clone())?;
        add_type(&mut lhs);
        let mut rhs = init.expr.clone().unwrap();
        add_type(&mut rhs);
        let mut node = Expr::new_assign(lhs, rhs, tok.pos);
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
    fn local_var_initializer(&mut self, var: Rc<RefCell<Obj>>) -> Result<Expr> {
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
        let lhs = Expr::new_memzero(var.clone(), tok.pos.clone());
        let rhs =
            self.create_local_var_init(&mut init, var.borrow().ty.clone(), &mut desg, tok.clone())?;
        Ok(Expr::new_comma(lhs, rhs, tok.pos.clone()))
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
            for mem in &mut ty.get_members() {
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
                &mut ty.get_members().get(0).unwrap().ty.clone(),
                buf,
                offset,
            );
        }

        if let Some(init_expr) = &mut init.expr {
            if ty.is_float() {
                let val = self.eval_double(init_expr)? as f32;
                let val = val.to_bits();
                buf[offset as usize] = (val >> 0) as u8;
                buf[(offset + 1) as usize] = (val >> 8) as u8;
                buf[(offset + 2) as usize] = (val >> 16) as u8;
                buf[(offset + 3) as usize] = (val >> 24) as u8;
                return Ok(vec![]);
            }
            if ty.is_double() {
                let val = self.eval_double(init_expr)?;
                let val = val.to_bits();
                buf[offset as usize] = val as u8;
                buf[(offset + 1) as usize] = (val >> 8) as u8;
                buf[(offset + 2) as usize] = (val >> 16) as u8;
                buf[(offset + 3) as usize] = (val >> 24) as u8;
                buf[(offset + 4) as usize] = (val >> 32) as u8;
                buf[(offset + 5) as usize] = (val >> 40) as u8;
                buf[(offset + 6) as usize] = (val >> 48) as u8;
                buf[(offset + 7) as usize] = (val >> 56) as u8;
                return Ok(vec![]);
            }
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

    /// stmt = "return" expr? ";"
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
                if self.peek()?.token == Semicolon {
                    eat!(self, Semicolon);
                    let node = WithPos::new(Stmt::Return { expr: None }, pos);
                    return Ok(node);
                }
                let mut expr = self.expr()?;
                add_type(&mut expr);
                let return_ty = self.current_fn.as_mut().unwrap().get_return_ty().unwrap();
                expr = Expr::new_cast_expr(expr, return_ty, pos);
                let node = WithPos::new(Stmt::Return { expr: Some(expr) }, pos);
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

                self.scope.enter_scope();

                let old_break_label = self.break_label.clone();
                self.break_label = Some(self.new_unique_name());

                let old_continue_label = self.continue_label.clone();
                self.continue_label = Some(self.new_unique_name());

                let tok = self.peek()?.token.clone();
                let init = if self.is_typename(tok)? {
                    let mut basety = self.declspec(&mut None)?;
                    self.declaration(&mut basety, &None)?
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

                self.scope.leave_scope();

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
            Tok::KeywordDo => {
                let pos = eat!(self, KeywordDo);
                let old_break_label = self.break_label.clone();
                self.break_label = Some(self.new_unique_name());
                let old_continue_label = self.continue_label.clone();
                self.continue_label = Some(self.new_unique_name());

                let final_brk_label = self.break_label.clone();
                let final_cont_label = self.continue_label.clone();

                let body = self.stmt()?;

                self.break_label = old_break_label;
                self.continue_label = old_continue_label;

                eat!(self, KeywordWhile);
                eat!(self, LeftParen);
                let cond = self.expr()?;
                eat!(self, RightParen);
                eat!(self, Semicolon);
                let node = WithPos::new(
                    Stmt::DoWhileStmt {
                        condition: cond,
                        body: Box::new(body),
                        break_label: final_brk_label,
                        continue_label: final_cont_label,
                    },
                    pos,
                );
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
    fn declaration(&mut self, basety: &mut Type, attr: &Option<VarAttr>) -> Result<StmtWithPos> {
        let pos = self.peek()?.pos.clone();
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
                    let mut ty = self.declarator(basety)?;
                    if ty.is_void() {
                        panic!("{:#?} variable declared void.", ty);
                    }
                    if ty.name.is_none() {
                        panic!("{:#?} variable name omitted.", ty);
                    }
                    if let Some(attr) = attr {
                        if attr.is_static() {
                            let unique_name = self.new_unique_name();
                            let var = self.new_global_variable(
                                unique_name,
                                &mut ty,
                                Some(InitData::IntInitData(0)),
                            )?;
                            if self.peek()?.token == Equal {
                                eat!(self, Equal);
                                self.global_var_initializer(var.clone())?;
                            }

                            self.scope.enter_var(ty.get_ident().unwrap(), var);
                            continue;
                        }
                    }
                    let ident = ty.get_ident().unwrap();
                    let var = self.new_local_variable(ident.clone(), &mut ty)?;
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

                    if var.borrow().ty.get_size() < 0 {
                        panic!("variable {} has incomplete type", var.borrow());
                    }
                    if var.borrow().ty.is_void() {
                        panic!("variable {} declared void", var.borrow());
                    }
                }
            }
        }

        Ok(WithPos::new(Stmt::Block { body: decls }, pos))
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
                    let mut basety = self.declspec(&mut attr)?;
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
                                let member_ty = self.declarator(&mut basety)?;
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
                    if let Some(t) = self.scope.find_tag(struct_tag.clone()) {
                        return Ok(t.clone());
                    } else {
                        let ty = Type::struct_type(tag_token, vec![], -1, 1);
                        self.scope.enter_tag(struct_tag, ty.clone());
                        return Ok(ty);
                    }
                }
            }
        }

        let mut ty = Type::struct_type(tag_token, vec![], 0, 1);
        self.struct_members(&mut ty)?;

        if let Some(tag) = tag {
            // If this is a redefinition, overwrite a previous type.
            // Otherwise, register the struct type.
            if self.scope.find_tag_in_current_scope(tag.clone()).is_some() {
                self.scope
                    .replace_tag_in_current_scope(tag.clone(), ty.clone());
                if let Some(obj) = self.scope.find_var(tag) {
                    if obj.borrow().type_def.is_some() {
                        obj.borrow_mut().type_def = Some(ty.clone());
                    }
                }
                return Ok(ty);
            }

            self.scope.enter_tag(tag, ty.clone());
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
        let mut members = ty.get_members();
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

        if let Some(tag) = ty.get_ident() {
            // If this is a redefinition, overwrite a previous type.
            // Otherwise, register the struct type.
            if self.scope.find_tag_in_current_scope(tag.clone()).is_some() {
                self.scope
                    .replace_tag_in_current_scope(tag.clone(), ty.clone());
                if let Some(obj) = self.scope.find_var(tag) {
                    if obj.borrow().type_def.is_some() {
                        obj.borrow_mut().type_def = Some(ty.clone());
                    }
                }
                return Ok(ty);
            }
        }

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
        let mut members = ty.get_members();
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
    ///          | "signed"
    ///          | struct-decl | union-decl | typedef-name
    ///          | enum-specifier
    ///          | "const" | "volatile" | "auto" | "register" | "restrict"
    ///          | "__restrict" | "__restrict__" | "_Noreturn")+
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
            FLOAT = 1 << 12,
            DOUBLE = 1 << 14,
            OTHER = 1 << 16,
            SIGNED = 1 << 17,
            UNSIGNED = 1 << 18,
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
                Tok::KeywordConst
                | Tok::KeywordVolatile
                | Tok::KeywordAuto
                | Tok::KeywordRegister
                | Tok::KeywordRestrict
                | Tok::KeywordRestrict1
                | Tok::KeywordRestrict2
                | Tok::KeywordNoreturn => {
                    self.token()?;
                    continue;
                }
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
                    if let Some(obj) = self.scope.find_var(name) {
                        if let Some(_ty) = obj.borrow().type_def.clone() {
                            ty = _ty;
                            counter += Counter::OTHER as i32;
                            continue;
                        }
                    }
                    panic!()
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
                Tok::KeywordSigned => {
                    eat!(self, KeywordSigned);
                    counter |= Counter::SIGNED as i32;
                }
                Tok::KeywordUnsigned => {
                    eat!(self, KeywordUnsigned);
                    counter |= Counter::UNSIGNED as i32;
                }
                Tok::KeywordFloat => {
                    eat!(self, KeywordFloat);
                    counter |= Counter::FLOAT as i32;
                }
                Tok::KeywordDouble => {
                    eat!(self, KeywordDouble);
                    counter |= Counter::DOUBLE as i32;
                }
                _ => unreachable!(),
            }

            if counter == Counter::VOID as i32 {
                ty = Type::new_void();
            } else if counter == Counter::BOOL as i32 {
                ty = Type::new_bool();
            } else if counter == Counter::CHAR as i32 {
                ty = Type::new_char();
            } else if counter == Counter::SIGNED as i32 + Counter::CHAR as i32 {
                ty = Type::new_char();
            } else if counter == Counter::UNSIGNED as i32 + Counter::CHAR as i32 {
                ty = Type::new_uchar();
            } else if counter == Counter::SHORT as i32 {
                ty = Type::new_short();
            } else if counter == Counter::SHORT as i32 + Counter::INT as i32 {
                ty = Type::new_short();
            } else if counter == Counter::SIGNED as i32 + Counter::SHORT as i32 {
                ty = Type::new_short();
            } else if counter
                == Counter::SIGNED as i32 + Counter::SHORT as i32 + Counter::INT as i32
            {
                ty = Type::new_short();
            } else if counter == Counter::UNSIGNED as i32 + Counter::SHORT as i32 {
                ty = Type::new_ushort();
            } else if counter
                == Counter::UNSIGNED as i32 + Counter::SHORT as i32 + Counter::INT as i32
            {
                ty = Type::new_ushort();
            } else if counter == Counter::INT as i32 {
                ty = Type::new_int();
            } else if counter == Counter::SIGNED as i32 {
                ty = Type::new_int();
            } else if counter == Counter::SIGNED as i32 + Counter::INT as i32 {
                ty = Type::new_int();
            } else if counter == Counter::UNSIGNED as i32 {
                ty = Type::new_uint();
            } else if counter == Counter::UNSIGNED as i32 + Counter::INT as i32 {
                ty = Type::new_uint();
            } else if counter == Counter::LONG as i32 + Counter::LONG as i32 + Counter::INT as i32 {
                ty = Type::new_long();
            } else if counter == Counter::LONG as i32 + Counter::INT as i32 {
                ty = Type::new_long();
            } else if counter == Counter::LONG as i32 + Counter::LONG as i32 {
                ty = Type::new_long();
            } else if counter == Counter::LONG as i32 {
                ty = Type::new_long();
            } else if counter == Counter::SIGNED as i32 + Counter::LONG as i32 {
                ty = Type::new_long();
            } else if counter == Counter::SIGNED as i32 + Counter::LONG as i32 + Counter::INT as i32
            {
                ty = Type::new_long();
            } else if counter
                == Counter::SIGNED as i32 + Counter::LONG as i32 + Counter::LONG as i32
            {
                ty = Type::new_long();
            } else if counter
                == Counter::SIGNED as i32
                    + Counter::LONG as i32
                    + Counter::LONG as i32
                    + Counter::INT as i32
            {
                ty = Type::new_long();
            } else if counter == Counter::UNSIGNED as i32 + Counter::LONG as i32 {
                ty = Type::new_ulong();
            } else if counter
                == Counter::UNSIGNED as i32 + Counter::LONG as i32 + Counter::INT as i32
            {
                ty = Type::new_ulong();
            } else if counter
                == Counter::UNSIGNED as i32 + Counter::LONG as i32 + Counter::LONG as i32
            {
                ty = Type::new_ulong();
            } else if counter
                == Counter::UNSIGNED as i32
                    + Counter::LONG as i32
                    + Counter::LONG as i32
                    + Counter::INT as i32
            {
                ty = Type::new_ulong();
            } else if counter == Counter::FLOAT as i32 {
                ty = Type::new_float();
            } else if counter == Counter::DOUBLE as i32 {
                ty = Type::new_double();
            } else if counter == Counter::LONG as i32 + Counter::DOUBLE as i32 {
                ty = Type::new_double();
            } else {
                panic!("invalid type.");
            }
        }

        Ok(ty)
    }

    /// func-params = ("void" | param ("," param)* ("," "...")?)? ")"
    /// param       = declspec declarator
    fn func_params(&mut self, ty: &mut Type) -> Result<Type> {
        let mut params = vec![];
        let mut is_variadic = false;
        if self.peek()?.token == KeywordVoid && self.peek_next_one()?.token == RightParen {
            eat!(self, KeywordVoid);
            eat!(self, RightParen);
            return Ok(Type::new_func(params, ty.clone(), false));
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
                    if self.peek()?.token == DotDotDot {
                        is_variadic = true;
                        eat!(self, DotDotDot);
                        eat!(self, RightParen);
                        break;
                    }
                    let mut ty2 = self.declspec(&mut None)?;
                    ty2 = self.declarator(&mut ty2)?;

                    let type_name = ty2.clone().name;
                    if ty2.is_array() {
                        // "array of T" is converted to "pointer to T" only in the parameter
                        // context. For example, *argv[] is converted to **argv by this.
                        ty2 = pointer_to(ty2.base().unwrap());
                        ty2.name = type_name;
                    } else if ty2.is_func() {
                        // Likewise, a function is converted to a pointer to a function
                        // only in the parameter context.
                        ty2 = pointer_to(ty2);
                        ty2.name = type_name;
                    }

                    params.push(ty2);
                }
            }
        }

        if params.len() == 0 {
            is_variadic = true;
        }

        let node = Type::new_func(params, ty.clone(), is_variadic);
        Ok(node)
    }

    /// type-suffix = "(" func-params
    ///             | "[" array-dimensions
    ///             | ε
    fn type_suffix(&mut self, ty: &mut Type) -> Result<Type> {
        match self.peek()?.token {
            Tok::LeftParen => {
                eat!(self, LeftParen);
                self.func_params(ty)
            }
            Tok::LeftBracket => {
                eat!(self, LeftBracket);
                return self.array_dimensions(ty);
            }
            _ => Ok(ty.clone()),
        }
    }

    /// pointers = ("*" ("const" | "volatile" | "restrict")*)*
    fn pointers(&mut self, ty: &mut Type) -> Result<Type> {
        let mut ty = ty.clone();
        while self.peek()?.token == Star {
            eat!(self, Star);
            ty = pointer_to(ty);
            loop {
                match self.peek()?.token {
                    Tok::KeywordConst
                    | Tok::KeywordVolatile
                    | Tok::KeywordRestrict
                    | Tok::KeywordRestrict1
                    | Tok::KeywordRestrict2 => {
                        self.token()?;
                    }
                    _ => break,
                }
            }
        }

        Ok(ty)
    }

    /// declarator = pointers ("(" ident ")" | "(" declarator ")" | ident) type-suffix
    fn declarator(&mut self, ty: &mut Type) -> Result<Type> {
        let mut ty = self.pointers(ty)?;

        if self.peek()?.token == LeftParen {
            let start = self.current_pos;
            let mut dummy = Type::new_placeholder();
            self.current_pos = start + 1;
            self.declarator(&mut dummy)?;
            eat!(self, RightParen);
            ty = self.type_suffix(&mut ty)?;
            let after_suffix = self.current_pos;
            self.current_pos = start + 1;
            let ty = self.declarator(&mut ty);
            self.current_pos = after_suffix;
            return ty;
        }

        let name = if self.peek()?.token.is_ident() {
            let tok = self.peek()?.clone();
            self.token()?;
            Some(tok)
        } else {
            None
        };

        ty = self.type_suffix(&mut ty)?;
        ty.name = name;

        return Ok(ty);
    }

    fn is_typename(&mut self, tok: Tok) -> Result<bool> {
        match tok {
            Tok::KeywordLong
            | Tok::KeywordInt
            | Tok::KeywordEnum
            | Tok::KeywordBool
            | Tok::KeywordChar
            | Tok::KeywordShort
            | Tok::KeywordFloat
            | Tok::KeywordDouble
            | Tok::KeywordStruct
            | Tok::KeywordUnion
            | Tok::KeywordTypedef
            | Tok::KeywordStatic
            | Tok::KeywordExtern
            | Tok::KeywordAlignas
            | Tok::KeywordSigned
            | Tok::KeywordUnsigned
            | Tok::KeywordConst
            | Tok::KeywordVolatile
            | Tok::KeywordAuto
            | Tok::KeywordRegister
            | Tok::KeywordRestrict
            | Tok::KeywordRestrict1
            | Tok::KeywordRestrict2
            | Tok::KeywordNoreturn
            | Tok::KeywordVoid => Ok(true),
            Tok::Ident(name) => {
                let obj = self.scope.find_var(name);
                if let Some(obj) = obj {
                    return Ok(obj.borrow().type_def.is_some());
                } else {
                    return Ok(false);
                }
            }
            _ => Ok(false),
        }
    }

    /// compound-stmt = (declaration | stmt)* "}"
    fn compound_stmt(&mut self) -> Result<StmtWithPos> {
        let pos = self.peek()?.pos.clone();
        let mut stmts = vec![];

        self.scope.enter_scope();
        loop {
            match self.peek()?.token {
                Tok::RightBrace => {
                    eat!(self, RightBrace);
                    break;
                }
                _ => {
                    // { typedef int t; t t=1; t t=2; t; }
                    let tok = self.peek()?.token.clone();
                    if self.is_typename(tok)? && self.peek_next_one()?.token != Colon {
                        let mut attr = Some(VarAttr::new_placeholder());
                        let mut basety = self.declspec(&mut attr)?;
                        if let Some(attr) = attr.clone() {
                            if attr.is_typedef() {
                                self.parse_typedef(&mut basety)?;
                                continue;
                            }
                        }

                        let pos = self.current_pos;
                        if self.is_function_definition()? {
                            self.current_pos = pos;
                            self.function(&mut basety, &attr)?;
                            continue;
                        } else {
                            self.current_pos = pos;
                        }
                        if attr.clone().unwrap().is_extern() {
                            self.global_variable(&mut basety, &attr)?;
                            continue;
                        }
                        let mut declaration = self.declaration(&mut basety, &attr)?;
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
        self.scope.leave_scope();
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

    /// expr = assign ("," expr)?
    fn expr(&mut self) -> Result<Expr> {
        let node = self.assign()?;
        match self.peek()?.token {
            Tok::Comma => {
                let pos = eat!(self, Comma);
                let node = Expr::new_comma(node, self.expr()?, pos);
                Ok(node)
            }
            _ => Ok(node),
        }
    }

    /// Convert `A op= B` to `tmp = &A, *tmp = *tmp op B`
    /// where tmp is a fresh pointer variable.
    fn to_assign(&mut self, binary: Expr) -> Result<Expr> {
        match binary.expr {
            ExprInner::Binary {
                mut left,
                op,
                mut right,
            } => {
                add_type(&mut left);
                add_type(&mut right);

                let var =
                    self.new_local_variable("".to_string(), &mut pointer_to(left.ty.clone()))?;

                // tmp = &A
                // tmp
                let mut l_value = Expr::new_var(var.clone(), left.pos.clone());
                add_type(&mut l_value);
                let mut r_value = Expr::new_addr(*left.clone(), left.pos.clone());
                add_type(&mut r_value);
                let mut expr1 = Expr::new_assign(l_value.clone(), r_value, left.pos.clone());
                add_type(&mut expr1);

                // *tmp = *tmp op B
                let mut l_value = Expr::new_deref(l_value, left.pos);
                add_type(&mut l_value);
                let mut r_value = Expr::new_binary(op.node, l_value.clone(), *right, left.pos);
                add_type(&mut r_value);
                let mut expr2 = Expr::new_assign(l_value, r_value, left.pos);
                add_type(&mut expr2);

                let mut node = Expr::new_comma(expr1, expr2, left.pos);
                add_type(&mut node);

                Ok(node)
            }
            _ => panic!(),
        }
    }

    /// assign    = conditional (assign-op assign)?
    /// assign-op = "=" | "+=" | "-=" | "*=" | "/=" | "%=" | "&=" | "|=" | "^="
    ///           | "<<=" | ">>="
    fn assign(&mut self) -> Result<Expr> {
        let mut expr = self.conditional()?;
        add_type(&mut expr);
        match self.peek()?.token {
            Tok::Equal => {
                let pos = eat!(self, Equal);
                let r_value = self.assign()?;
                expr = Expr::new_assign(expr, r_value, pos);
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
                expr = Expr::new_binary(ast::BinaryOperator::Mul, expr, rhs, pos);
                expr = self.to_assign(expr)?;
            }
            Tok::SlashEqual => {
                let pos = eat!(self, SlashEqual);
                let rhs = self.assign()?;
                expr = Expr::new_binary(ast::BinaryOperator::Div, expr, rhs, pos);
                expr = self.to_assign(expr)?;
            }
            Tok::PercentEqual => {
                let pos = eat!(self, PercentEqual);
                let rhs = self.assign()?;
                expr = Expr::new_binary(ast::BinaryOperator::Mod, expr, rhs, pos);
                expr = self.to_assign(expr)?;
            }
            Tok::AmpEqual => {
                let pos = eat!(self, AmpEqual);
                let rhs = self.assign()?;
                expr = Expr::new_binary(ast::BinaryOperator::BitAnd, expr, rhs, pos);
                expr = self.to_assign(expr)?;
            }
            Tok::BarEqual => {
                let pos = eat!(self, BarEqual);
                let rhs = self.assign()?;
                expr = Expr::new_binary(ast::BinaryOperator::BitOr, expr, rhs, pos);
                expr = self.to_assign(expr)?;
            }
            Tok::HatEqual => {
                let pos = eat!(self, HatEqual);
                let rhs = self.assign()?;
                expr = Expr::new_binary(ast::BinaryOperator::BitXor, expr, rhs, pos);
                expr = self.to_assign(expr)?;
            }
            Tok::LesserLesserEqual => {
                let pos = eat!(self, LesserLesserEqual);
                let rhs = self.assign()?;
                expr = Expr::new_binary(ast::BinaryOperator::SHL, expr, rhs, pos);
                expr = self.to_assign(expr)?;
            }
            Tok::GreaterGreaterEqual => {
                let pos = eat!(self, GreaterGreaterEqual);
                let rhs = self.assign()?;
                expr = Expr::new_binary(ast::BinaryOperator::SHR, expr, rhs, pos);
                expr = self.to_assign(expr)?;
            }
            _ => (),
        }
        Ok(expr)
    }

    /// equality = relational ("==" relational | "!=" relational)*
    fn equality(&mut self) -> Result<Expr> {
        let mut expr = self.relational()?;

        loop {
            match self.peek()?.token {
                Tok::EqualEqual => {
                    let pos = eat!(self, EqualEqual);
                    let right = self.relational()?;
                    expr = Expr::new_binary(ast::BinaryOperator::Eq, expr, right, pos);
                }
                Tok::BangEqual => {
                    let pos = eat!(self, BangEqual);
                    let right = self.relational()?;
                    expr = Expr::new_binary(ast::BinaryOperator::Ne, expr, right, pos);
                }
                _ => break,
            }
        }
        Ok(expr)
    }

    /// relational = shift ("<" shift | "<=" shift | ">" shift | ">=" shift)*
    fn relational(&mut self) -> Result<Expr> {
        let mut expr = self.shift()?;

        loop {
            match self.peek()?.token {
                Tok::Lesser => {
                    let pos = eat!(self, Lesser);
                    let right = self.shift()?;
                    expr = Expr::new_binary(ast::BinaryOperator::Lt, expr, right, pos);
                }
                Tok::LesserEqual => {
                    let pos = eat!(self, LesserEqual);
                    let right = self.shift()?;
                    expr = Expr::new_binary(ast::BinaryOperator::Le, expr, right, pos);
                }
                Tok::Greater => {
                    let pos = eat!(self, Greater);
                    let right = self.shift()?;
                    expr = Expr::new_binary(ast::BinaryOperator::Gt, expr, right, pos);
                }
                Tok::GreaterEqual => {
                    let pos = eat!(self, GreaterEqual);
                    let right = self.shift()?;
                    expr = Expr::new_binary(ast::BinaryOperator::Ge, expr, right, pos);
                }
                _ => break,
            }
        }

        Ok(expr)
    }

    /// add = mul ("+" mul | "-" mul)*
    fn add(&mut self) -> Result<Expr> {
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
    fn shift(&mut self) -> Result<Expr> {
        let mut node = self.add()?;
        add_type(&mut node);

        loop {
            match self.peek()?.token {
                Tok::LesserLesser => {
                    let pos = eat!(self, LesserLesser);
                    let rhs = self.add()?;
                    node = Expr::new_binary(BinaryOperator::SHL, node, rhs, pos);
                    add_type(&mut node);
                }
                Tok::GreaterGreater => {
                    let pos = eat!(self, GreaterGreater);
                    let rhs = self.add()?;
                    node = Expr::new_binary(BinaryOperator::SHR, node, rhs, pos);
                    add_type(&mut node);
                }
                _ => break,
            }
        }

        Ok(node)
    }

    /// conditional = logor ("?" expr ":" conditional)?
    fn conditional(&mut self) -> Result<Expr> {
        let condition = self.logor()?;
        match self.peek()?.token {
            Tok::QuestionMark => {
                let pos = eat!(self, QuestionMark);
                let then_clause = self.expr()?;
                eat!(self, Colon);
                let else_clause = self.conditional()?;
                let node = Expr::new_ternary(condition, then_clause, else_clause, pos);
                Ok(node)
            }
            _ => Ok(condition),
        }
    }

    /// logor = logand ("||" logand)*
    fn logor(&mut self) -> Result<Expr> {
        let mut node = self.logand()?;

        loop {
            match self.peek()?.token {
                Tok::BarBar => {
                    let pos = eat!(self, BarBar);
                    let rhs = self.logand()?;
                    node = Expr::new_binary(BinaryOperator::LogOr, node, rhs, pos);
                }
                _ => break,
            }
        }

        Ok(node)
    }

    /// logand = bitor ("&&" bitor)*
    fn logand(&mut self) -> Result<Expr> {
        let mut node = self.bitor()?;

        loop {
            match self.peek()?.token {
                Tok::AmpAmp => {
                    let pos = eat!(self, AmpAmp);
                    let rhs = self.bitor()?;
                    node = Expr::new_binary(BinaryOperator::LogAnd, node, rhs, pos);
                }
                _ => break,
            }
        }

        Ok(node)
    }

    /// bitor = bitxor ("|" bitxor)*
    fn bitor(&mut self) -> Result<Expr> {
        let mut node = self.bitxor()?;

        loop {
            match self.peek()?.token {
                Tok::Bar => {
                    let pos = eat!(self, Bar);
                    let rhs = self.bitxor()?;
                    node = Expr::new_binary(BinaryOperator::BitOr, node, rhs, pos);
                }
                _ => break,
            }
        }

        Ok(node)
    }

    /// bitxor = bitand ("^" bitand)*
    fn bitxor(&mut self) -> Result<Expr> {
        let mut node = self.bitand()?;

        loop {
            match self.peek()?.token {
                Tok::Hat => {
                    let pos = eat!(self, Hat);
                    let rhs = self.bitand()?;
                    node = Expr::new_binary(BinaryOperator::BitXor, node, rhs, pos);
                }
                _ => break,
            }
        }

        Ok(node)
    }

    /// bitand = equality ("&" equality)*
    fn bitand(&mut self) -> Result<Expr> {
        let mut node = self.equality()?;

        loop {
            match self.peek()?.token {
                Tok::Amp => {
                    let pos = eat!(self, Amp);
                    let rhs = self.equality()?;
                    node = Expr::new_binary(BinaryOperator::BitAnd, node, rhs, pos);
                }
                _ => break,
            }
        }

        Ok(node)
    }

    /// mul = cast ("*" cast | "/" cast)*
    fn mul(&mut self) -> Result<Expr> {
        let mut node = self.cast()?;

        loop {
            match self.peek()?.token {
                Tok::Star => {
                    let pos = eat!(self, Star);
                    let rhs = self.cast()?;
                    node = Expr::new_binary(BinaryOperator::Mul, node, rhs, pos);
                    add_type(&mut node);
                }
                Tok::Slash => {
                    let pos = eat!(self, Slash);
                    let rhs = self.cast()?;
                    node = Expr::new_binary(BinaryOperator::Div, node, rhs, pos);
                    add_type(&mut node);
                }
                Tok::Percent => {
                    let pos = eat!(self, Percent);
                    let rhs = self.cast()?;
                    node = Expr::new_binary(BinaryOperator::Mod, node, rhs, pos);
                    add_type(&mut node);
                }
                _ => break,
            }
        }

        Ok(node)
    }

    /// unary = ("+" | "-" | "*" | "&" | "!" | "~") cast
    ///       | ("++" | "--") unary
    ///       | postfix
    fn unary(&mut self) -> Result<Expr> {
        match self.peek()?.token {
            Plus => {
                eat!(self, Plus);
                Ok(self.cast()?)
            }
            Minus => {
                let op = WithPos::new(UnaryOperator::Neg, eat!(self, Minus));
                let expr = self.cast()?;
                let pos = expr.pos.grow(expr.pos);
                Ok(Expr::new_unary(op, expr, pos))
            }
            Amp => {
                let pos = eat!(self, Amp);
                let mut expr = self.cast()?;
                add_type(&mut expr);
                Ok(Expr::new_addr(expr, pos))
            }
            Star => {
                let pos = eat!(self, Star);
                let mut expr = self.cast()?;
                add_type(&mut expr);
                Ok(Expr::new_deref(expr, pos))
            }
            Bang => {
                let op = WithPos::new(UnaryOperator::Not, eat!(self, Bang));
                let expr = self.cast()?;
                let pos = expr.pos.grow(expr.pos);
                Ok(Expr::new_unary(op, expr, pos))
            }
            Tilde => {
                let op = WithPos::new(UnaryOperator::BitNot, eat!(self, Tilde));
                let expr = self.cast()?;
                let pos = expr.pos.grow(expr.pos);
                Ok(Expr::new_unary(op, expr, pos))
            }
            // read ++i as i+=1
            PlusPlus => {
                let pos = eat!(self, PlusPlus);
                let i = self.unary()?;
                let one = Expr::new_int(1, pos);
                let binary = self.new_add(i, one, pos)?;
                self.to_assign(binary)
            }
            // read --i as i-=1
            MinusMinus => {
                let pos = eat!(self, MinusMinus);
                let i = self.unary()?;
                let one = Expr::new_int(1, pos);
                let binary = self.new_sub(i, one, pos)?;
                self.to_assign(binary)
            }
            _ => self.postfix(),
        }
    }

    /// cast = "(" type-name ")" cast | unary
    fn cast(&mut self) -> Result<Expr> {
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
                        // 回溯
                        self.current_pos = old_pos;
                        return self.unary();
                    }

                    // type cast
                    let node = Expr::new_cast_expr(self.cast()?, ty, pos);
                    Ok(node)
                } else {
                    self.unary()
                }
            }
            _ => self.unary(),
        }
    }

    fn new_sub(&mut self, mut lhs: Expr, mut rhs: Expr, pos: Pos) -> Result<Expr> {
        add_type(&mut lhs);
        add_type(&mut rhs);

        match (lhs.ty.clone().ty, rhs.ty.clone().ty) {
            // num - num
            _ if lhs.ty.is_number() && rhs.ty.is_number() => {
                lhs = Expr::new_binary(BinaryOperator::Sub, lhs, rhs, pos);
            }
            // ptr - num
            (Ty::TyPtr { base, .. } | Ty::TyArray { base, .. }, Ty::TyInt | Ty::TyLong) => {
                // num * 8
                let num = Expr::new_long(base.get_size() as i64, pos);
                rhs = Expr::new_binary(BinaryOperator::Mul, rhs, num, pos);
                lhs = Expr::new_binary(BinaryOperator::Sub, lhs, rhs, pos);
            }
            // ptr - ptr, which returns how many elements are between the two.
            (Ty::TyPtr { base, .. }, Ty::TyPtr { .. }) => {
                let num = Expr::new_long(base.get_size() as i64, pos);
                lhs = Expr::new_binary(BinaryOperator::Sub, lhs, rhs, pos);
                lhs = Expr::new_binary(BinaryOperator::Div, lhs, num, pos);
            }
            // other
            _ => panic!(
                "invalid operands for pointer arithmetic sub. {:?} ===== {:?}",
                lhs, rhs
            ),
        }

        Ok(lhs)
    }

    fn new_add(&mut self, mut lhs: Expr, mut rhs: Expr, pos: Pos) -> Result<Expr> {
        add_type(&mut lhs);
        add_type(&mut rhs);

        match (lhs.ty.clone().ty, rhs.ty.clone().ty) {
            // ptr + num
            (Ty::TyPtr { base, .. } | Ty::TyArray { base, .. }, _) if rhs.ty.is_integer() => {
                // num * 8
                let num = Expr::new_long(base.get_size() as i64, pos);
                rhs = Expr::new_binary(ast::BinaryOperator::Mul, rhs, num, pos);
                lhs = Expr::new_binary_with_type(
                    ast::BinaryOperator::Add,
                    lhs.clone(),
                    rhs,
                    pos,
                    lhs.ty,
                );
            }
            // num + ptr
            (_, Ty::TyPtr { base, .. } | Ty::TyArray { base, .. }) if lhs.ty.is_integer() => {
                // num * 8
                let num = Expr::new_long(base.get_size() as i64, pos);
                lhs = Expr::new_binary(ast::BinaryOperator::Mul, lhs, num, pos);
                lhs = Expr::new_binary_with_type(
                    ast::BinaryOperator::Add,
                    rhs.clone(),
                    lhs,
                    pos,
                    rhs.ty,
                );
            }
            // num + num
            _ if lhs.ty.is_number() && rhs.ty.is_number() => {
                lhs = Expr::new_binary(ast::BinaryOperator::Add, lhs, rhs, pos);
            }
            // other
            _ => panic!("invalid operands for pointer arithmetic add."),
        }

        Ok(lhs)
    }

    fn struct_ref(&mut self, node: &mut Expr, name: String) -> Result<Expr> {
        add_type(node);
        if !node.ty.is_struct() && !node.ty.is_union() {
            panic!("not a struct nor a union.")
        }

        if let Some(member) = node.ty.get_member(name.clone()) {
            let node = Expr::new_member_expr(node.clone(), member, self.peek()?.pos);
            return Ok(node);
        } else if let Some(tk) = &node.ty.name {
            let struct_name = tk.token.get_ident_name().unwrap();
            if let Some(t) = &mut self.scope.find_tag(struct_name) {
                if let Some(member) = t.get_member(name.clone()) {
                    let node = Expr::new_member_expr(node.clone(), member, self.peek()?.pos);
                    return Ok(node);
                }
            }
        }

        panic!("{:?} ===> {}", node, name)
    }

    /// Convert A++ to `(typeof A)((A += 1) - 1)`
    /// Convert A-- to `(typeof A)((A -= 1) + 1)`
    fn new_inc_dec(&mut self, mut node: Expr, addend: i32) -> Result<Expr> {
        add_type(&mut node);

        let pos = self.peek()?.pos;

        let positive_one = Expr::new_int(addend as i32, pos);
        let negative_one = Expr::new_int(-addend as i32, pos);

        // A += 1
        // A + 1
        let a_plus_one = self.new_add(node.clone(), positive_one, pos)?;
        // A = A + 1
        let a = self.to_assign(a_plus_one)?;
        let mut e = self.new_add(a, negative_one, pos)?;
        add_type(&mut e);

        // cast
        let c = Expr::new_cast_expr(e, node.ty, pos);

        Ok(c)
    }

    /// postfix = "(" type-name ")" "{" initializer-list "}"
    ///         | primary ("[" expr "]" | "." ident | "->" ident | "++" | "--")*
    fn postfix(&mut self) -> Result<Expr> {
        let current_token = self.peek()?.token.clone();
        let next_token = self.peek_next_one()?.token.clone();
        if current_token == LeftParen && self.is_typename(next_token)? {
            // Compound literal
            let pos = eat!(self, LeftParen);
            let mut ty = self.typename()?;
            eat!(self, RightParen);

            if self.scope.is_parent_env_empty() {
                let unique_name = self.new_unique_name();
                let var =
                    self.new_global_variable(unique_name, &mut ty, Some(InitData::IntInitData(0)))?;
                self.global_var_initializer(var.clone())?;
                let node = Expr::new_var(var, pos);
                return Ok(node);
            }

            let var = self.new_local_variable("".to_string(), &mut ty)?;
            let lhs = self.local_var_initializer(var.clone())?;
            let mut rhs = Expr::new_var(var, pos);
            add_type(&mut rhs);
            return Ok(Expr::new_comma(lhs, rhs, pos));
        }

        let mut node = self.primary()?;

        loop {
            match self.peek()?.token {
                Tok::LeftParen => {
                    eat!(self, LeftParen);
                    node = self.funcall(&mut node)?;
                }
                Tok::LeftBracket => {
                    // x[y] is short for *(x+y)
                    let pos = eat!(self, LeftBracket);
                    let idx = self.expr()?;
                    eat!(self, RightBracket);
                    node = Expr::new_deref(self.new_add(node, idx, pos)?, pos);
                }
                Tok::Dot => {
                    eat!(self, Dot);
                    add_type(&mut node);
                    let name;
                    eat!(self, Ident, name);
                    node = self.struct_ref(&mut node, name)?;
                }
                Tok::MinusGreater => {
                    // x->y is short for (*x).y
                    let pos = eat!(self, MinusGreater);
                    add_type(&mut node);
                    node = Expr::new_deref(node, pos);
                    let name;
                    eat!(self, Ident, name);
                    node = self.struct_ref(&mut node, name)?;
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

    /// funcall = (assign ("," assign)*)? ")"
    fn funcall(&mut self, func: &mut Expr) -> Result<Expr> {
        add_type(func);

        if !func.ty.is_func() {
            if !func.ty.is_ptr() {
                panic!("{:?} not a function", func.ty);
            }
            if let Some(base) = func.ty.base() {
                if !base.is_func() {
                    panic!("{:?} not a function", func.ty);
                }
            }
        }

        let ty = if func.ty.is_func() {
            func.ty.clone()
        } else {
            func.ty.clone().base().unwrap()
        };

        let params_ty = match ty.clone().ty {
            Ty::TyFunc { params, .. } => params,
            _ => unreachable!(),
        };

        let mut args = vec![];

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
                    if params_ty.is_empty() && !func.clone().ty.is_variadic() {
                        panic!("too many arguments");
                    }

                    if let Some(param_type) = params_ty.get(i) {
                        if arg_exp.ty.is_union() || arg_exp.ty.is_struct() {
                            panic!("passing struct or union is not supported yet.");
                        }

                        arg_exp = Expr::new_cast_expr(
                            arg_exp.clone(),
                            param_type.clone(),
                            arg_exp.pos.clone(),
                        );
                    } else if arg_exp.ty.is_float() {
                        // If parameter type is omitted (e.g. in "..."), float
                        // arguments are promoted to double.
                        arg_exp = Expr::new_cast_expr(
                            arg_exp.clone(),
                            Type::new_double(),
                            arg_exp.pos.clone(),
                        );
                    }

                    args.push(arg_exp);

                    i += 1;
                }
            }
        }

        if i < params_ty.len() {
            panic!("too few arguments");
        }

        let node = Expr {
            expr: ExprInner::FunctionCall {
                expr: Box::new(func.clone()),
                args,
            },
            ty: match ty.ty {
                Ty::TyFunc { return_ty, .. } => *return_ty,
                _ => unreachable!(),
            },
            pass_by_stack: false,
            pos: func.pos,
        };
        Ok(node)
    }

    /// abstract-declarator = pointers ("(" abstract-declarator ")")? type-suffix
    fn abstract_declarator(&mut self, ty: &mut Type) -> Result<Type> {
        *ty = self.pointers(ty)?;

        match self.peek()?.token {
            Tok::LeftParen => {
                eat!(self, LeftParen);
                self.abstract_declarator(&mut Type::new_placeholder())?;
                eat!(self, RightParen);
                *ty = self.type_suffix(ty)?;
                Ok(Type::new_ptr(ty.clone()))
            }
            _ => self.type_suffix(ty),
        }
    }

    /// type-name = declspec abstract-declarator
    fn typename(&mut self) -> Result<Type> {
        let mut ty = self.declspec(&mut None)?;
        self.abstract_declarator(&mut ty)
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
    fn primary(&mut self) -> Result<Expr> {
        match self.peek()?.token {
            LeftParen => {
                eat!(self, LeftParen);
                match self.peek()?.token {
                    // This is a GNU statement expression.
                    Tok::LeftBrace => {
                        let pos = eat!(self, LeftBrace);
                        let stmt = self.compound_stmt()?.node;
                        let node = match stmt {
                            Stmt::Block { body } => ExprInner::StmtExpr { body: body.clone() },
                            _ => panic!(),
                        };
                        eat!(self, RightParen);
                        Ok(Expr {
                            expr: node,
                            ty: Type::new_placeholder(),
                            pass_by_stack: false,
                            pos,
                        })
                    }
                    _ => {
                        let expr = self.expr()?;
                        eat!(self, RightParen);
                        Ok(expr)
                    }
                }
            }
            ConstLong(_) => {
                let value;
                let pos = eat!(self, ConstLong, value);
                let node = Expr::new_long(value, pos);
                Ok(node)
            }
            ConstInt(_) => {
                let value;
                let pos = eat!(self, ConstInt, value);
                let node = Expr::new_int(value, pos);
                Ok(node)
            }
            ConstULong(_) => {
                let value;
                let pos = eat!(self, ConstULong, value);
                let node = Expr::new_ulong(value, pos);
                Ok(node)
            }
            ConstUInt(_) => {
                let value;
                let pos = eat!(self, ConstUInt, value);
                let node = Expr::new_uint(value, pos);
                Ok(node)
            }
            ConstFloat(_) => {
                let value;
                let pos = eat!(self, ConstFloat, value);
                let node = Expr::new_float(value, pos);
                Ok(node)
            }
            ConstDouble(_) => {
                let value;
                let pos = eat!(self, ConstDouble, value);
                let node = Expr::new_double(value, pos);
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

                            Ok(Expr::new_ulong(ty.get_size() as u64, pos))
                        }
                        // sizeof(x)
                        else {
                            let mut node = self.unary()?;
                            add_type(&mut node);
                            node = Expr::new_ulong(node.ty.get_size() as u64, pos);
                            Ok(node)
                        }
                    }
                    // sizeof x
                    _ => {
                        let mut node = self.unary()?;
                        add_type(&mut node);
                        node = Expr::new_ulong(node.ty.get_size() as u64, pos);
                        Ok(node)
                    }
                }
            }
            Tok::Ident(_) => {
                let name;
                let pos = eat!(self, Ident, name);
                // variable
                if let Some(var) = self.scope.find_var(name.clone()) {
                    let _ty = var.borrow_mut().ty.clone();
                    match _ty.ty {
                        Ty::TyEnum => {
                            if let Some(InitData::IntInitData(i)) = var.borrow_mut().init_data {
                                return Ok(Expr::new_long(i as i64, pos));
                            } else {
                                let node = Expr::new_var(var.clone(), pos);
                                return Ok(node);
                            }
                        }
                        _ => {
                            let mut node = Expr::new_var(var.clone(), pos);
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
                let mut var_type = Type::new_array(Type::new_char(), string.len() as i32);
                let var = self.new_global_variable(
                    unique_name,
                    &mut var_type,
                    Some(InitData::StringInitData(string)),
                )?;
                let mut node = Expr::new_var(var.clone(), pos);
                add_type(&mut node);
                return Ok(node);
            }
            Tok::KeywordAlignof => {
                eat!(self, KeywordAlignof);
                let next_token = self.peek_next_one()?.token.clone();
                if self.peek()?.token == LeftParen && self.is_typename(next_token)? {
                    eat!(self, LeftParen);
                    let pos = self.peek()?.pos;
                    let ty = self.typename()?;
                    eat!(self, RightParen);
                    return Ok(Expr::new_ulong(ty.get_align() as u64, pos));
                } else {
                    let mut node = self.unary()?;
                    add_type(&mut node);
                    return Ok(Expr::new_ulong(node.ty.get_align() as u64, node.pos));
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

    fn function(&mut self, basety: &mut Type, attr: &Option<VarAttr>) -> Result<()> {
        let mut ty = self.declarator(basety)?;
        if ty.name.is_none() {
            panic!("function name omitted.");
        }

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

        let func = self.new_global_variable(
            ty.clone().name.unwrap().token.get_ident_name().unwrap(),
            &mut ty,
            None,
        )?;
        func.borrow_mut().is_definition = is_definition;
        func.borrow_mut().is_static = is_static;

        if !func.borrow().is_definition {
            return Ok(());
        }

        self.locals = Vec::new();

        self.scope.enter_scope();

        let ident;
        match ty.clone().ty {
            Ty::TyFunc { ref mut params, .. } => {
                ident = ty.get_ident().unwrap();
                let pos = self.peek()?.pos.clone();
                // 为了处理fib这样的递归调用，需要先把函数声明记录下来。
                let mut fndef =
                    ast::FunctionDefinition::new(ident.clone(), is_static, ty.clone(), pos);
                self.functions.push(fndef.clone());

                self.current_fn = Some(fndef.clone());

                for p in params {
                    if p.name.is_none() {
                        panic!("parameter name omitted.");
                    }
                    let param_name = p.get_ident().unwrap();
                    self.new_local_variable(param_name, p)?;
                }

                // 先将函数的参数拷贝一份，因为后面在解析函数体时会加入新的局部变量
                let params = self.locals.clone();

                if is_definition {
                    eat!(self, LeftBrace);
                    fndef.va_area = if ty.is_variadic() {
                        Some(self.new_local_variable(
                            "__va_area__".to_string(),
                            &mut Type::new_array(Type::new_char(), 136),
                        )?)
                    } else {
                        None
                    };
                    fndef.body = self.compound_stmt()?;
                    fndef.is_definition = is_definition;
                    fndef.params = params;
                    fndef.locals = self.locals.clone();
                    fndef.goto_labels = self.goto_labels.clone();
                }

                self.scope.leave_scope();

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
                let mut dummy = Type::new_placeholder();
                let ty = self.declarator(&mut dummy)?;
                Ok(ty.is_func())
            }
        }
    }

    fn new_var(&mut self, name: String, ty: &mut Type) -> Result<Rc<RefCell<Obj>>> {
        let var = Rc::new(RefCell::new(Obj {
            name: Some(name.clone()),
            ty: ty.clone(),
            offset: 0,
            is_local: false,
            is_static: false,
            is_definition: false,
            init_data: None,
            rel: vec![],
            align: ty.get_align(),
            type_def: None,
        }));
        self.scope.enter_var(name, var.clone());
        Ok(var)
    }

    fn new_global_variable(
        &mut self,
        name: String,
        ty: &mut Type,
        init_value: Option<InitData>,
    ) -> Result<Rc<RefCell<Obj>>> {
        let var = self.new_var(name, ty)?;
        var.borrow_mut().is_local = false;
        var.borrow_mut().is_static = true;
        var.borrow_mut().is_definition = true;
        var.borrow_mut().init_data = init_value;
        self.globals.insert(0, var.clone());
        Ok(var)
    }

    fn new_local_variable(&mut self, name: String, ty: &mut Type) -> Result<Rc<RefCell<Obj>>> {
        let var = self.new_var(name, ty)?;
        var.borrow_mut().is_local = true;
        self.locals.insert(0, var.clone());
        Ok(var)
    }

    fn global_variable(&mut self, basety: &mut Type, attr: &Option<VarAttr>) -> Result<()> {
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
                    let mut ty = self.declarator(basety)?;
                    if ty.name.is_none() {
                        panic!("variable name omitted.");
                    }
                    let var_name = ty.get_ident().unwrap();
                    if self.peek()?.token.is_ident() {
                        continue;
                    }
                    let var = self.new_global_variable(var_name, &mut ty, None)?;
                    if let Some(attr) = attr {
                        var.borrow_mut().is_definition = !attr.is_extern();
                        var.borrow_mut().is_static = attr.is_static();
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

    fn parse_typedef(&mut self, basety: &mut Type) -> Result<()> {
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
                    let ty = self.declarator(basety)?;
                    if ty.name.is_none() {
                        panic!("typedef name omitted.");
                    }
                    let ident = ty.get_ident().unwrap();
                    let var = Rc::new(RefCell::new(Obj {
                        name: Some(ident.clone()),
                        ty: ty.clone(),
                        offset: 0,
                        is_local: false,
                        is_static: false,
                        is_definition: false,
                        init_data: None,
                        rel: vec![],
                        align: ty.get_align(),
                        type_def: Some(ty),
                    }));
                    self.scope.enter_var(ident, var);
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
            if let Some(ty) = self
                .scope
                .find_tag(tag.unwrap().token.get_ident_name().unwrap())
            {
                if !ty.is_enum() {
                    panic!("not an enum tag.");
                } else {
                    return Ok(ty);
                }
            } else {
                panic!("unknown enum type.");
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
                name: Some(name.clone()),
                offset: 0,
                is_local: false,
                is_static: false,
                is_definition: false,
                init_data: Some(InitData::IntInitData(val as i32)),
                ty: ty.clone(),
                rel: vec![],
                align: 0,
                type_def: None,
            }));
            val += 1;
            self.scope.enter_var(name, enum_var);
        }

        if let Some(tag) = tag {
            self.scope
                .enter_tag(tag.token.get_ident_name().unwrap(), ty.clone());
        }
        Ok(ty)
    }

    /// array-dimensions = ("static" | "restrict")* const-expr? "]" type-suffix
    fn array_dimensions(&mut self, ty: &mut Type) -> Result<Type> {
        loop {
            match self.peek()?.token {
                Tok::KeywordRestrict | Tok::KeywordStatic => {
                    self.token()?;
                }
                _ => break,
            }
        }
        match self.peek()?.token {
            Tok::RightBracket => {
                eat!(self, RightBracket);
                *ty = self.type_suffix(ty)?;
                Ok(Type::new_array(ty.clone(), -1))
            }
            _ => {
                let sz = self.const_expr()?;
                eat!(self, RightBracket);
                *ty = self.type_suffix(ty)?;
                Ok(Type::new_array(ty.clone(), sz as i32))
            }
        }
    }

    fn const_expr(&mut self) -> Result<i64> {
        let mut node = self.conditional()?;
        Ok(self.eval_constexpr(&mut node)?)
    }

    fn eval_constexpr(&mut self, node: &mut Expr) -> Result<i64> {
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
        node: &mut Expr,
        label: &mut Option<String>,
    ) -> Result<i64> {
        add_type(node);

        if node.ty.is_flonum() {
            return Ok(self.eval_double(node)? as i64);
        }

        match node.expr.clone() {
            ExprInner::Binary {
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
                    if node.ty.is_unsigned() && node.ty.get_size() == 8 {
                        return Ok(((self.eval_constexpr(&mut left)? as u64)
                            / (self.eval_constexpr(&mut right)? as u64))
                            as i64);
                    }
                    Ok(self.eval_constexpr(&mut left)? / self.eval_constexpr(&mut right)?)
                }
                ast::BinaryOperator::Mod => {
                    if node.ty.is_unsigned() && node.ty.get_size() == 8 {
                        return Ok(((self.eval_constexpr(&mut left)? as u64)
                            % (self.eval_constexpr(&mut right)? as u64))
                            as i64);
                    }
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
                    if node.ty.is_unsigned() && node.ty.get_size() == 8 {
                        return Ok(((self.eval_constexpr(&mut left)? as u64)
                            >> (self.eval_constexpr(&mut right)? as u64))
                            as i64);
                    }
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
            ExprInner::Unary { op, mut expr } => match op.node {
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
            ExprInner::CommaExpr { mut right, .. } => {
                Ok(self.eval_constexpr_with_label(&mut right, label)?)
            }
            ExprInner::CastExpr { mut expr, ty } => {
                let val = self.eval_constexpr_with_label(&mut expr, label)?;
                match ty.is_integer() {
                    true => match ty.get_size() {
                        1 if ty.is_unsigned() => Ok(val as u8 as i64),
                        1 => Ok(val as i8 as i64),
                        2 if ty.is_unsigned() => Ok(val as u16 as i64),
                        2 => Ok(val as i16 as i64),
                        4 if ty.is_unsigned() => Ok(val as u32 as i64),
                        4 => Ok(val as i32 as i64),
                        _ => Ok(val),
                    },
                    false => Ok(val),
                }
            }
            ExprInner::TernaryExpr {
                mut condition,
                mut then_clause,
                mut else_clause,
            } => match self.eval_constexpr(&mut condition)? {
                0 => Ok(self.eval_constexpr_with_label(&mut else_clause, label)?),
                _ => Ok(self.eval_constexpr_with_label(&mut then_clause, label)?),
            },
            ExprInner::Addr { mut expr } => self.eval_rvalue(&mut expr, label),
            ExprInner::MemberExpr { mut strct, member } => {
                if !node.ty.is_array() {
                    panic!()
                }
                return Ok(self.eval_rvalue(&mut strct, label)? + member.offset as i64);
            }
            ExprInner::Variable { obj } => {
                if !obj.borrow().ty.is_array() && !obj.borrow().ty.is_func() {
                    panic!()
                }
                *label = Some(obj.borrow().name.clone().unwrap());
                return Ok(0);
            }
            ExprInner::ConstInt { value } => Ok(value as i64),
            ExprInner::ConstUInt { value } => Ok(value as i64),
            ExprInner::ConstLong { value } => Ok(value as i64),
            ExprInner::ConstULong { value } => Ok(value as i64),
            _ => panic!("not a compile-time constant. {:?}", node.clone()),
        }
    }

    fn eval_rvalue(&mut self, node: &mut Expr, label: &mut Option<String>) -> Result<i64> {
        match node.expr.clone() {
            ExprInner::Variable { obj } => {
                if obj.borrow().is_local {
                    panic!("{} not a compile-time constant", obj.borrow());
                }
                *label = Some(obj.borrow().name.clone().unwrap());
                return Ok(0);
            }
            ExprInner::Deref { mut expr } => {
                return self.eval_constexpr_with_label(&mut expr, label)
            }
            ExprInner::MemberExpr { mut strct, member } => {
                return Ok(self.eval_rvalue(&mut strct, label)? + member.offset as i64);
            }
            _ => return Err(Error::InvalidInitializer { pos: node.pos }),
        }
    }

    fn eval_double(&mut self, node: &mut Expr) -> Result<f64> {
        add_type(node);

        if node.ty.is_integer() {
            if node.ty.is_unsigned() {
                return Ok(self.eval_constexpr(node)? as u64 as f64);
            }
            return Ok(self.eval_constexpr(node)? as f64);
        }

        match node.expr.clone() {
            ExprInner::Binary {
                mut left,
                op,
                mut right,
            } => match op.node {
                BinaryOperator::Add => {
                    return Ok(self.eval_double(&mut left)? + self.eval_double(&mut right)?)
                }
                BinaryOperator::Sub => {
                    return Ok(self.eval_double(&mut left)? - self.eval_double(&mut right)?)
                }
                BinaryOperator::Mul => {
                    return Ok(self.eval_double(&mut left)? * self.eval_double(&mut right)?)
                }
                BinaryOperator::Div => {
                    let val = self.eval_double(&mut left)? / self.eval_double(&mut right)?;
                    return Ok(val);
                }
                _ => panic!(),
            },
            ExprInner::Unary { op, mut expr } => match op.node {
                UnaryOperator::Neg => return Ok(-self.eval_double(&mut expr)?),
                _ => panic!(),
            },
            ExprInner::CommaExpr { left: _, mut right } => return self.eval_double(&mut right),
            ExprInner::TernaryExpr {
                mut condition,
                mut then_clause,
                mut else_clause,
            } => {
                let val = self.eval_double(&mut condition)?;
                if val != 0.0 {
                    return self.eval_double(&mut then_clause);
                } else {
                    return self.eval_double(&mut else_clause);
                }
            }
            ExprInner::CastExpr { mut expr, .. } => {
                if expr.ty.is_flonum() {
                    return self.eval_double(&mut expr);
                }
                return Ok(self.eval_constexpr(&mut expr)? as f64);
            }
            ExprInner::ConstDouble { value } => return Ok(value),
            ExprInner::ConstFloat { value } => return Ok(value as f64),
            _ => panic!(),
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
                    let mut basety = self.declspec(&mut attr)?;
                    if let Some(attr) = attr.clone() {
                        if attr.is_typedef() {
                            self.parse_typedef(&mut basety)?;
                            continue;
                        }
                    }
                    // 保存当前的位置
                    let pos = self.current_pos;
                    if self.is_function_definition()? {
                        // 回溯
                        self.current_pos = pos;
                        self.function(&mut basety, &attr)?;
                    } else {
                        // 回溯
                        self.current_pos = pos;
                        self.global_variable(&mut basety, &attr)?;
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
