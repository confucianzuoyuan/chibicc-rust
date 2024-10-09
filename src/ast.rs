use std::{cell::RefCell, collections::HashMap, rc::Rc};

use crate::position::WithPos;

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
    Variable(Rc<RefCell<Obj>>),
}

pub type ExprWithPos = WithPos<Expr>;

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
}

pub type BinaryOperatorWithPos = WithPos<BinaryOperator>;

#[derive(Clone, Copy, Debug, PartialEq)]
pub enum UnaryOperator {
    Neg,
}

pub type UnaryOperatorWithPos = WithPos<UnaryOperator>;

#[derive(Clone, Debug, PartialEq)]
pub enum Stmt {
    ExprStmt { expr: ExprWithPos },
    Return { expr: ExprWithPos },
    Block { body: Vec<StmtWithPos> },
    NullStmt,
}

pub type StmtWithPos = WithPos<Stmt>;

pub type Program = Function;

#[derive(Clone, Debug, PartialEq)]
pub struct Obj {
    pub name: String,
    pub offset: i32,
}

#[derive(Clone, Debug, PartialEq)]
pub struct Function {
    pub body: Vec<StmtWithPos>,
    pub locals: HashMap<String, Rc<RefCell<Obj>>>,
    pub stack_size: i32,
}
