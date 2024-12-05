use std::{cell::RefCell, collections::HashMap, fmt::Display, rc::Rc};

use crate::{
    position::{Pos, WithPos},
    sema::{Member, Ty, Type},
    token::Token,
};

#[derive(Clone, Debug, PartialEq)]
pub enum ExprInner {
    Binary {
        left: Box<Expr>,
        op: BinaryOperatorWithPos,
        right: Box<Expr>,
    },
    Unary {
        op: UnaryOperatorWithPos,
        expr: Box<Expr>,
    },
    ConstInt {
        value: i32,
    },
    ConstUInt {
        value: u32,
    },
    ConstLong {
        value: i64,
    },
    ConstULong {
        value: u64,
    },
    ConstFloat {
        value: f32,
    },
    ConstDouble {
        value: f64,
    },
    Assign {
        l_value: Box<Expr>,
        r_value: Box<Expr>,
    },
    Variable {
        obj: Rc<RefCell<Obj>>,
    },
    Deref {
        expr: Box<Expr>,
    },
    Addr {
        expr: Box<Expr>,
    },
    FunctionCall {
        expr: Box<Expr>,
        args: Vec<Expr>,
    },
    StmtExpr {
        body: Vec<StmtWithPos>,
    },
    CommaExpr {
        left: Box<Expr>,
        right: Box<Expr>,
    },
    /// . (struct member access)
    MemberExpr {
        strct: Box<Expr>,
        member: Member,
    },
    CastExpr {
        expr: Box<Expr>,
        ty: Type,
    },
    TernaryExpr {
        condition: Box<Expr>,
        then_clause: Box<Expr>,
        else_clause: Box<Expr>,
    },
    /// Zero-clear a stack variable
    MemZero {
        var: Rc<RefCell<Obj>>,
    },
    Null,
}

#[derive(Clone, Debug, PartialEq)]
pub struct Expr {
    pub expr: ExprInner,
    pub ty: Type,
    pub pos: Pos,
}

impl Expr {
    pub fn new_cast_expr(expr: Expr, ty: Type, pos: Pos) -> Self {
        Expr {
            expr: ExprInner::CastExpr {
                expr: Box::new(expr),
                ty: ty.clone(),
            },
            ty,
            pos,
        }
    }

    pub fn new_member_expr(strct: Expr, member: Member, pos: Pos) -> Self {
        Expr {
            expr: ExprInner::MemberExpr {
                strct: Box::new(strct),
                member,
            },
            ty: Type::new_placeholder(),
            pos,
        }
    }

    pub fn new_memzero(var: Rc<RefCell<Obj>>, pos: Pos) -> Self {
        Expr {
            expr: ExprInner::MemZero { var },
            ty: Type::new_placeholder(),
            pos,
        }
    }

    pub fn new_null_expr(pos: Pos) -> Self {
        Expr {
            expr: ExprInner::Null,
            ty: Type::new_placeholder(),
            pos,
        }
    }

    pub fn new_float(i: f32, pos: Pos) -> Self {
        Expr {
            expr: ExprInner::ConstFloat { value: i },
            ty: Type::new_float(),
            pos,
        }
    }

    pub fn new_double(i: f64, pos: Pos) -> Self {
        Expr {
            expr: ExprInner::ConstDouble { value: i },
            ty: Type::new_double(),
            pos,
        }
    }

    pub fn new_int(i: i32, pos: Pos) -> Self {
        Expr {
            expr: ExprInner::ConstInt { value: i },
            ty: Type::new_int(),
            pos,
        }
    }

    pub fn new_uint(i: u32, pos: Pos) -> Self {
        Expr {
            expr: ExprInner::ConstUInt { value: i },
            ty: Type::new_uint(),
            pos,
        }
    }

    pub fn new_long(i: i64, pos: Pos) -> Self {
        Expr {
            expr: ExprInner::ConstLong { value: i },
            ty: Type::new_long(),
            pos,
        }
    }

    pub fn new_ulong(i: u64, pos: Pos) -> Self {
        Expr {
            expr: ExprInner::ConstULong { value: i },
            ty: Type::new_ulong(),
            pos,
        }
    }

    pub fn new_var(obj: Rc<RefCell<Obj>>, pos: Pos) -> Self {
        Expr {
            expr: ExprInner::Variable { obj },
            ty: Type::new_placeholder(),
            pos,
        }
    }

    pub fn new_deref(expr: Expr, pos: Pos) -> Self {
        Expr {
            expr: ExprInner::Deref {
                expr: Box::new(expr),
            },
            ty: Type::new_placeholder(),
            pos,
        }
    }

    pub fn new_addr(expr: Expr, pos: Pos) -> Self {
        Expr {
            expr: ExprInner::Addr {
                expr: Box::new(expr),
            },
            ty: Type::new_placeholder(),
            pos,
        }
    }

    pub fn new_comma(left: Expr, right: Expr, pos: Pos) -> Self {
        Expr {
            expr: ExprInner::CommaExpr {
                left: Box::new(left),
                right: Box::new(right),
            },
            ty: Type::new_placeholder(),
            pos,
        }
    }

    pub fn new_assign(left: Expr, right: Expr, pos: Pos) -> Self {
        Expr {
            expr: ExprInner::Assign {
                l_value: Box::new(left),
                r_value: Box::new(right),
            },
            ty: Type::new_placeholder(),
            pos,
        }
    }

    pub fn new_unary(op: UnaryOperatorWithPos, expr: Expr, pos: Pos) -> Self {
        Expr {
            expr: ExprInner::Unary {
                op,
                expr: Box::new(expr),
            },
            ty: Type::new_placeholder(),
            pos,
        }
    }

    pub fn new_binary(op: BinaryOperator, left: Expr, right: Expr, pos: Pos) -> Self {
        Expr {
            expr: ExprInner::Binary {
                left: Box::new(left),
                op: WithPos::new(op, pos),
                right: Box::new(right),
            },
            ty: Type::new_placeholder(),
            pos,
        }
    }

    pub fn new_binary_with_type(
        op: BinaryOperator,
        left: Expr,
        right: Expr,
        pos: Pos,
        ty: Type,
    ) -> Self {
        Expr {
            expr: ExprInner::Binary {
                left: Box::new(left),
                op: WithPos::new(op, pos),
                right: Box::new(right),
            },
            ty,
            pos,
        }
    }

    pub fn new_ternary(condition: Expr, then_clause: Expr, else_clause: Expr, pos: Pos) -> Self {
        Expr {
            expr: ExprInner::TernaryExpr {
                then_clause: Box::new(then_clause),
                else_clause: Box::new(else_clause),
                condition: Box::new(condition),
            },
            ty: Type::new_placeholder(),
            pos,
        }
    }
}

#[derive(Clone, Copy, Debug, PartialEq)]
pub enum BinaryOperator {
    Add,
    Sub,
    Mul,
    Div,
    Eq,
    Ne,
    Lt,
    Le,
    Gt,
    Ge,
    Mod,
    BitAnd,
    BitOr,
    BitXor,
    LogAnd,
    LogOr,
    SHL,
    SHR,
}

pub type BinaryOperatorWithPos = WithPos<BinaryOperator>;

impl Display for BinaryOperatorWithPos {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let string = (|| {
            let string = match *self {
                WithPos { node, .. } => match node {
                    BinaryOperator::Add => "+",
                    BinaryOperator::Div => "/",
                    BinaryOperator::Eq => "==",
                    BinaryOperator::Ge => ">=",
                    BinaryOperator::Gt => ">",
                    BinaryOperator::Le => "<=",
                    BinaryOperator::Lt => "<",
                    BinaryOperator::Mul => "*",
                    BinaryOperator::Ne => "!=",
                    BinaryOperator::Sub => "-",
                    BinaryOperator::Mod => "%",
                    BinaryOperator::BitAnd => "&",
                    BinaryOperator::BitOr => "|",
                    BinaryOperator::BitXor => "^",
                    BinaryOperator::LogAnd => "&&",
                    BinaryOperator::LogOr => "||",
                    BinaryOperator::SHL => "<<",
                    BinaryOperator::SHR => ">>",
                },
            };
            string.to_string()
        })();
        write!(f, "{}", string)
    }
}

#[derive(Clone, Copy, Debug, PartialEq)]
pub enum UnaryOperator {
    Neg,
    Not,
    BitNot,
}

pub type UnaryOperatorWithPos = WithPos<UnaryOperator>;

impl Display for UnaryOperatorWithPos {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let string = (|| {
            let string = match *self {
                WithPos { node, .. } => match node {
                    UnaryOperator::Neg => "-",
                    UnaryOperator::Not => "!",
                    UnaryOperator::BitNot => "~",
                },
            };
            string.to_string()
        })();
        write!(f, "{}", string)
    }
}

#[derive(Clone, Debug, PartialEq)]
pub enum Stmt {
    ExprStmt {
        expr: Expr,
    },
    Return {
        expr: Option<Expr>,
    },
    Block {
        body: Vec<StmtWithPos>,
    },
    NullStmt,
    IfStmt {
        condition: Expr,
        then_clause: Box<StmtWithPos>,
        else_clause: Option<Box<StmtWithPos>>,
    },
    ForStmt {
        init: Box<StmtWithPos>,
        condition: Option<Expr>,
        body: Box<StmtWithPos>,
        increment: Option<Expr>,
        break_label: Option<String>,
        continue_label: Option<String>,
    },
    WhileStmt {
        condition: Expr,
        body: Box<StmtWithPos>,
        break_label: Option<String>,
        continue_label: Option<String>,
    },
    DoWhileStmt {
        condition: Expr,
        body: Box<StmtWithPos>,
        break_label: Option<String>,
        continue_label: Option<String>,
    },
    GotoStmt {
        label: String,
    },
    LabelStmt {
        label: String,
        stmt: Box<StmtWithPos>,
    },
    SwitchStmt {
        condition: Expr,
        cases: Vec<StmtWithPos>,
        default_case: Option<Box<StmtWithPos>>,
        break_label: Option<String>,
        body: Option<Box<StmtWithPos>>,
    },
    CaseStmt {
        label: String,
        val: i64,
        stmt: Box<StmtWithPos>,
    },
}

pub type StmtWithPos = WithPos<Stmt>;

impl StmtWithPos {
    pub fn get_val(&self) -> i64 {
        match self.node {
            Stmt::CaseStmt { val, .. } => val,
            _ => 0,
        }
    }

    pub fn get_label(&self) -> String {
        match self.node.clone() {
            Stmt::CaseStmt { label, .. } => label,
            _ => String::new(),
        }
    }

    pub fn new_switch(condition: Expr, pos: Pos) -> Self {
        WithPos::new(
            Stmt::SwitchStmt {
                condition,
                cases: vec![],
                default_case: None,
                break_label: None,
                body: None,
            },
            pos,
        )
    }

    pub fn update_break_label(&mut self, brk_label: Option<String>) {
        match self.node {
            Stmt::SwitchStmt {
                ref mut break_label,
                ..
            } => *break_label = brk_label,
            _ => (),
        }
    }

    pub fn update_body(&mut self, new_body: StmtWithPos) {
        match self.node {
            Stmt::SwitchStmt { ref mut body, .. } => *body = Some(Box::new(new_body)),
            _ => (),
        }
    }

    pub fn update_cases(&mut self, new_cases: Vec<StmtWithPos>) {
        match self.node {
            Stmt::SwitchStmt { ref mut cases, .. } => *cases = new_cases,
            _ => (),
        }
    }

    pub fn update_default_case(&mut self, new_default_case: Option<StmtWithPos>) {
        match self.node {
            Stmt::SwitchStmt {
                ref mut default_case,
                ..
            } => {
                *default_case = if let Some(dc) = new_default_case {
                    Some(Box::new(dc))
                } else {
                    None
                }
            }
            _ => (),
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub enum InitData {
    StringInitData(String),
    BytesInitData(Vec<u8>),
    IntInitData(i32),
}

#[derive(Clone, Debug, PartialEq)]
pub struct Obj {
    pub name: Option<String>,
    pub offset: i32,
    pub ty: Type,
    pub is_local: bool,
    pub is_static: bool,
    pub is_definition: bool,
    pub init_data: Option<InitData>,
    pub rel: Vec<Relocation>,
    /// alignment
    pub align: i32,
}

impl Display for Obj {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if let Some(name) = &self.name {
            write!(f, "{}", name)
        } else {
            write!(f, "no name")
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct Relocation {
    pub offset: i32,
    pub label: String,
    pub addend: i64,
}

#[derive(Clone, Debug, PartialEq)]
pub enum VarAttrKind {
    Typedef,
    Static,
    Extern,
    Placeholder,
}

#[derive(Clone, Debug, PartialEq)]
pub struct VarAttr {
    pub kind: VarAttrKind,
    pub align: i32,
}

impl VarAttr {
    pub fn is_extern(&self) -> bool {
        match self.kind {
            VarAttrKind::Extern => true,
            _ => false,
        }
    }

    pub fn is_static(&self) -> bool {
        match self.kind {
            VarAttrKind::Static => true,
            _ => false,
        }
    }

    pub fn is_typedef(&self) -> bool {
        match self.kind {
            VarAttrKind::Typedef => true,
            _ => false,
        }
    }

    pub fn new_placeholder() -> Self {
        Self {
            kind: VarAttrKind::Placeholder,
            align: 0,
        }
    }

    pub fn new_typedef() -> Self {
        Self {
            kind: VarAttrKind::Typedef,
            align: 0,
        }
    }

    pub fn new_static() -> Self {
        Self {
            kind: VarAttrKind::Static,
            align: 0,
        }
    }

    pub fn new_extern() -> Self {
        Self {
            kind: VarAttrKind::Extern,
            align: 0,
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct FunctionDefinition {
    pub name: Option<String>,
    pub params: Vec<Rc<RefCell<Obj>>>,
    pub body: StmtWithPos,
    pub locals: Vec<Rc<RefCell<Obj>>>,
    pub va_area: Option<Rc<RefCell<Obj>>>,
    pub stack_size: i32,
    pub is_definition: bool,
    pub ty: Type,
    pub is_static: bool,
    pub goto_labels: HashMap<String, String>,
}

impl FunctionDefinition {
    pub fn new(name: String, is_static: bool, ty: Type, pos: Pos) -> Self {
        Self {
            name: Some(name),
            params: vec![],
            body: WithPos::new(Stmt::NullStmt, pos),
            locals: vec![],
            va_area: None,
            stack_size: 0,
            is_definition: false,
            ty,
            is_static,
            goto_labels: HashMap::new(),
        }
    }

    pub fn get_return_ty(&self) -> Option<Type> {
        match &self.ty.ty {
            Ty::TyFunc { return_ty, .. } => Some(*return_ty.clone()),
            _ => None,
        }
    }
}

/// This struct represents a variable initializer. Since initializers
/// can be nested (e.g. `int x[2][2] = {{1, 2}, {3, 4}}`), this struct
/// is a tree data structure.
#[derive(Clone, Debug, PartialEq)]
pub struct Initializer {
    // If it's an initializer for an aggregate type (e.g. array or struct),
    // `children` has initializers for its children.
    pub children: Vec<Initializer>,
    pub ty: Type,
    // If it's not an aggregate type and has an initializer,
    // `expr` has an initialization expression.
    pub expr: Option<Expr>,
    pub is_flexible: bool,
    pub tok: Option<Token>,
}

impl Initializer {
    pub fn new(ty: Type, is_flexible: bool) -> Self {
        Self {
            children: vec![],
            ty,
            expr: None,
            tok: None,
            is_flexible,
        }
    }

    pub fn add_child(&mut self, child: Initializer) {
        self.children.push(child);
    }

    pub fn get_child(&mut self, n: i32) -> &mut Initializer {
        let children = &mut self.children;
        let child = children.get_mut(n as usize).unwrap();
        child
    }
}

/// For local variable initializer.
#[derive(Clone, Debug, PartialEq)]
pub struct InitDesg {
    pub idx: i64,
    pub var: Option<Rc<RefCell<Obj>>>,
    pub next: Option<Box<InitDesg>>,
    pub member: Option<Member>,
}

impl InitDesg {
    pub fn next(&mut self) -> &mut InitDesg {
        self.next.as_mut().unwrap()
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct Program {
    pub funcs: Vec<FunctionDefinition>,
    pub globals: Vec<Rc<RefCell<Obj>>>,
}
