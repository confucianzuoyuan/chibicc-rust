use std::{cell::RefCell, collections::HashMap, fmt::Display, rc::Rc};

use crate::{
    position::{Pos, WithPos},
    sema::{Member, Ty, Type, WithType},
    token::Token,
};

#[derive(Clone, Debug, PartialEq)]
pub enum Expr {
    Binary {
        left: Box<ExprWithPos>,
        op: BinaryOperatorWithPos,
        right: Box<ExprWithPos>,
    },
    Unary {
        op: UnaryOperatorWithPos,
        expr: Box<ExprWithPos>,
    },
    Number {
        value: i64,
    },
    Assign {
        l_value: Box<ExprWithPos>,
        r_value: Box<ExprWithPos>,
    },
    Variable {
        obj: Rc<RefCell<Obj>>,
    },
    Deref {
        expr: Box<ExprWithPos>,
    },
    Addr {
        expr: Box<ExprWithPos>,
    },
    FunctionCall {
        name: String,
        args: Vec<ExprWithPos>,
    },
    StmtExpr {
        body: Vec<StmtWithPos>,
    },
    CommaExpr {
        left: Box<ExprWithPos>,
        right: Box<ExprWithPos>,
    },
    /// . (struct member access)
    MemberExpr {
        strct: Box<ExprWithPos>,
        member: Member,
    },
    CastExpr {
        expr: Box<ExprWithPos>,
        ty: Type,
    },
    TernaryExpr {
        condition: Box<ExprWithPos>,
        then_clause: Box<ExprWithPos>,
        else_clause: Box<ExprWithPos>,
    },
    /// Zero-clear a stack variable
    MemZero {
        var: Rc<RefCell<Obj>>,
    },
    Null,
}

pub type ExprWithPos = WithPos<ExprWithType>;
pub type ExprWithType = WithType<Expr>;

impl ExprWithPos {
    pub fn new_member_expr(strct: ExprWithPos, member: Member, pos: Pos) -> Self {
        WithPos::new(
            WithType::new(
                Expr::MemberExpr {
                    strct: Box::new(strct),
                    member,
                },
                Type {
                    ty: Ty::TyPlaceholder,
                    name: None,
                },
            ),
            pos,
        )
    }

    pub fn new_memzero(var: Rc<RefCell<Obj>>, pos: Pos) -> Self {
        WithPos::new(
            WithType::new(
                Expr::MemZero { var },
                Type {
                    ty: Ty::TyPlaceholder,
                    name: None,
                },
            ),
            pos,
        )
    }

    pub fn new_null_expr(pos: Pos) -> Self {
        WithPos::new(
            WithType::new(
                Expr::Null,
                Type {
                    ty: Ty::TyPlaceholder,
                    name: None,
                },
            ),
            pos,
        )
    }

    pub fn new_number(i: i64, pos: Pos) -> Self {
        WithPos::new(
            WithType::new(
                Expr::Number { value: i },
                Type {
                    ty: Ty::TyLong,
                    name: None,
                },
            ),
            pos,
        )
    }

    pub fn new_var(obj: Rc<RefCell<Obj>>, pos: Pos) -> Self {
        WithPos::new(
            WithType::new(
                Expr::Variable { obj },
                Type {
                    ty: Ty::TyPlaceholder,
                    name: None,
                },
            ),
            pos,
        )
    }

    pub fn new_deref(expr: ExprWithPos, pos: Pos) -> Self {
        WithPos::new(
            WithType::new(
                Expr::Deref {
                    expr: Box::new(expr),
                },
                Type {
                    ty: Ty::TyPlaceholder,
                    name: None,
                },
            ),
            pos,
        )
    }

    pub fn new_addr(expr: ExprWithPos, pos: Pos) -> Self {
        WithPos::new(
            WithType::new(
                Expr::Addr {
                    expr: Box::new(expr),
                },
                Type {
                    ty: Ty::TyPlaceholder,
                    name: None,
                },
            ),
            pos,
        )
    }

    pub fn new_comma(left: ExprWithPos, right: ExprWithPos, pos: Pos) -> Self {
        WithPos::new(
            WithType::new(
                Expr::CommaExpr {
                    left: Box::new(left),
                    right: Box::new(right),
                },
                Type {
                    ty: Ty::TyPlaceholder,
                    name: None,
                },
            ),
            pos,
        )
    }

    pub fn new_assign(left: ExprWithPos, right: ExprWithPos, pos: Pos) -> Self {
        WithPos::new(
            WithType::new(
                Expr::Assign {
                    l_value: Box::new(left),
                    r_value: Box::new(right),
                },
                Type {
                    ty: Ty::TyPlaceholder,
                    name: None,
                },
            ),
            pos,
        )
    }

    pub fn new_binary(op: BinaryOperator, left: ExprWithPos, right: ExprWithPos, pos: Pos) -> Self {
        WithPos::new(
            WithType::new(
                Expr::Binary {
                    left: Box::new(left),
                    op: WithPos::new(op, pos),
                    right: Box::new(right),
                },
                Type {
                    ty: Ty::TyPlaceholder,
                    name: None,
                },
            ),
            pos,
        )
    }

    pub fn new_binary_with_type(
        op: BinaryOperator,
        left: ExprWithPos,
        right: ExprWithPos,
        pos: Pos,
        ty: Type,
    ) -> Self {
        WithPos::new(
            WithType::new(
                Expr::Binary {
                    left: Box::new(left),
                    op: WithPos::new(op, pos),
                    right: Box::new(right),
                },
                ty,
            ),
            pos,
        )
    }

    pub fn new_ternary(
        condition: ExprWithPos,
        then_clause: ExprWithPos,
        else_clause: ExprWithPos,
        pos: Pos,
    ) -> Self {
        WithPos::new(
            WithType::new(
                Expr::TernaryExpr {
                    then_clause: Box::new(then_clause),
                    else_clause: Box::new(else_clause),
                    condition: Box::new(condition),
                },
                Type {
                    ty: Ty::TyPlaceholder,
                    name: None,
                },
            ),
            pos,
        )
    }
}

impl Display for ExprWithPos {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let string = (|| {
            let string = match self {
                WithPos {
                    node: WithType { node, .. },
                    ..
                } => match node {
                    Expr::Number { value } => value.to_string(),
                    Expr::Binary { left, op, right } => {
                        return format!("{} {} {}", left, op, right)
                    }
                    Expr::Addr { expr } => return format!("get address of {}", expr),
                    Expr::Assign { l_value, r_value } => {
                        return format!("#{} assign to {}#", r_value, l_value)
                    }
                    Expr::CommaExpr { left, right } => return format!("{}, {}", left, right),
                    Expr::Deref { expr } => return format!("deref {}", expr),
                    Expr::FunctionCall { name, args } => {
                        let mut r = format!("funname {} args: ", name);
                        for arg in args {
                            r.push_str(format!(" {}", arg).as_str());
                        }
                        return r;
                    }
                    Expr::MemberExpr { strct, member } => return format!("{}.{:?}", strct, member),
                    Expr::StmtExpr { body } => {
                        let mut r = format!("stmt-expr");
                        for stmt in body {
                            r.push_str(format!(" {}", stmt).as_str());
                        }
                        return r;
                    }
                    Expr::Variable { obj } => {
                        return format!("var expr {}", obj.borrow());
                    }
                    Expr::Unary { op, expr } => return format!("{}{}", op, expr),
                    Expr::CastExpr { expr, ty } => {
                        return format!("cast expr {} to type {:?}", expr, ty);
                    }
                    Expr::TernaryExpr {
                        condition,
                        then_clause,
                        else_clause,
                    } => {
                        return format!("{} ? {} : {}", condition, then_clause, else_clause);
                    }
                    Expr::MemZero { var } => format!("{}", var.borrow()),
                    Expr::Null => format!("null expr"),
                },
            };
            string.to_string()
        })();
        write!(f, "{}", string)
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
        expr: ExprWithPos,
    },
    Return {
        expr: ExprWithPos,
    },
    Block {
        body: Vec<StmtWithPos>,
    },
    NullStmt,
    IfStmt {
        condition: ExprWithPos,
        then_clause: Box<StmtWithPos>,
        else_clause: Option<Box<StmtWithPos>>,
    },
    ForStmt {
        init: Box<StmtWithPos>,
        condition: Option<ExprWithPos>,
        body: Box<StmtWithPos>,
        increment: Option<ExprWithPos>,
        break_label: Option<String>,
        continue_label: Option<String>,
    },
    WhileStmt {
        condition: ExprWithPos,
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
        condition: ExprWithPos,
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

    pub fn new_switch(condition: ExprWithPos, pos: Pos) -> Self {
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

impl Display for StmtWithPos {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let string = (|| {
            let string = match self {
                WithPos { node, .. } => match node.clone() {
                    Stmt::Block { body } => {
                        let mut r = format!("block: ");
                        for stmt in body {
                            r.push_str(format!("{} ", stmt).as_str());
                        }
                        return r;
                    }
                    Stmt::ExprStmt { expr } => return format!("expr-stmt: {}", expr),
                    Stmt::ForStmt { .. } => return format!("{:?}", node),
                    Stmt::NullStmt => "null stmt.",
                    Stmt::IfStmt { .. } => return format!("{:?}", node),
                    Stmt::Return { expr } => return format!("return {}", expr),
                    Stmt::WhileStmt { .. } => return format!("{:?}", node),
                    Stmt::GotoStmt { .. } => return format!("{:?}", node),
                    Stmt::LabelStmt { .. } => return format!("{:?}", node),
                    Stmt::SwitchStmt { .. } => return format!("{:?}", node),
                    Stmt::CaseStmt { .. } => return format!("{:?}", node),
                },
            };
            string.to_string()
        })();
        writeln!(f, "{}", string)
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
    pub name: String,
    pub offset: i32,
    pub ty: Type,
    pub is_local: bool,
    pub init_data: Option<InitData>,
    pub rel: Vec<Relocation>,
}

impl Display for Obj {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.name)
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct Relocation {
    pub offset: i32,
    pub label: String,
    pub addend: i64,
}

#[derive(Clone, Debug, PartialEq)]
pub enum VarAttr {
    Typedef { type_def: Option<Type> },
    Static,
    Placeholder,
}

#[derive(Clone, Debug, PartialEq)]
pub struct Function {
    pub name: String,
    pub params: Vec<Rc<RefCell<Obj>>>,
    pub body: StmtWithPos,
    pub locals: Vec<Rc<RefCell<Obj>>>,
    pub stack_size: i32,
    pub is_definition: bool,
    pub ty: Type,
    pub is_static: bool,
    pub goto_labels: HashMap<String, String>,
}

impl Function {
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
    pub expr: Option<ExprWithPos>,
    pub is_flexible: bool,
    pub tok: Option<Token>,
}

impl Initializer {
    pub fn new(ty: Type) -> Self {
        Self {
            children: vec![],
            ty,
            expr: None,
            tok: None,
            is_flexible: false,
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
    pub funcs: Vec<Function>,
    pub globals: Vec<Rc<RefCell<Obj>>>,
}
