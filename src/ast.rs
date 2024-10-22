use std::{cell::RefCell, fmt::Display, rc::Rc};

use crate::{
    position::WithPos,
    sema::{Member, Type, WithType},
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
        member: Rc<RefCell<Member>>,
    },
    CastExpr {
        expr: Box<ExprWithPos>,
        ty: Type,
    },
}

pub type ExprWithPos = WithPos<ExprWithType>;
pub type ExprWithType = WithType<Expr>;

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
    },
    WhileStmt {
        condition: ExprWithPos,
        body: Box<StmtWithPos>,
    },
}

pub type StmtWithPos = WithPos<Stmt>;

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
    IntInitData(i32),
}

#[derive(Clone, Debug, PartialEq)]
pub struct Obj {
    pub name: String,
    pub offset: i32,
    pub ty: Type,
    pub is_local: bool,
    pub init_data: Option<InitData>,
}

impl Display for Obj {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.name)
    }
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
}

#[derive(Clone, Debug, PartialEq)]
pub struct Program {
    pub funcs: Vec<Function>,
    pub globals: Vec<Rc<RefCell<Obj>>>,
}
