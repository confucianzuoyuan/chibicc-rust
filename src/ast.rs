use std::{cell::RefCell, collections::HashMap, rc::Rc};

use crate::{
    position::WithPos,
    sema::{Type, WithType},
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
}

pub type ExprWithPos = WithPos<ExprWithType>;
pub type ExprWithType = WithType<Expr>;

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

#[derive(Clone, Debug, PartialEq)]
pub enum InitData {
    StringInitData(String),
}

#[derive(Clone, Debug, PartialEq)]
pub struct Obj {
    pub name: String,
    pub offset: i32,
    pub ty: Type,
    pub is_local: bool,
    pub init_data: Option<InitData>,
}

#[derive(Clone, Debug, PartialEq)]
pub struct Function {
    pub name: String,
    pub params: HashMap<String, Rc<RefCell<Obj>>>,
    pub body: StmtWithPos,
    pub locals: HashMap<String, Rc<RefCell<Obj>>>,
    pub stack_size: i32,
}

#[derive(Clone, Debug, PartialEq)]
pub struct Program {
    pub funcs: Vec<Function>,
    pub globals: HashMap<String, Rc<RefCell<Obj>>>,
}
