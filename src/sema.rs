use crate::ast::BinaryOperator::{Add, Div, Eq, Ge, Gt, Le, Lt, Mul, Ne, Sub};
use crate::ast::{self, BinaryOperatorWithPos, UnaryOperatorWithPos};

#[derive(Clone, Debug, PartialEq)]
pub enum Type {
    TyInt,
    TyPtr(Box<Type>),
    TyPlaceholder,
}

#[derive(Debug, Clone, PartialEq)]
pub struct WithType<T> {
    pub node: T,
    pub ty: Type,
}

impl<T> WithType<T> {
    pub fn new(node: T, ty: Type) -> Self {
        Self { node, ty }
    }
}

fn pointer_to(base: Type) -> Type {
    Type::TyPtr(Box::new(base))
}

pub fn add_type(e: &mut ast::ExprWithPos) {
    match &e.node.node {
        ast::Expr::Binary {
            op:
                BinaryOperatorWithPos {
                    node: Eq | Ne | Lt | Le | Ge | Gt,
                    ..
                },
            ..
        }
        | ast::Expr::Variable { .. }
        | ast::Expr::Number { .. } => {
            if e.node.ty == Type::TyPlaceholder {
                e.node.ty = Type::TyInt;
            }
        }
        ast::Expr::Binary {
            op:
                BinaryOperatorWithPos {
                    node: Add | Sub | Mul | Div,
                    ..
                },
            left,
            ..
        } => {
            if e.node.ty == Type::TyPlaceholder {
                e.node.ty = left.node.ty.clone();
            }
        }
        ast::Expr::Assign { l_value, .. } => {
            if e.node.ty == Type::TyPlaceholder {
                e.node.ty = l_value.node.ty.clone();
            }
        }
        ast::Expr::Unary {
            op:
                UnaryOperatorWithPos {
                    node: ast::UnaryOperator::Neg,
                    ..
                },
            expr,
        } => {
            if e.node.ty == Type::TyPlaceholder {
                e.node.ty = expr.node.ty.clone();
            }
        }
        ast::Expr::Addr { expr } => {
            if e.node.ty == Type::TyPlaceholder {
                e.node.ty = pointer_to(expr.node.ty.clone());
            }
        }
        ast::Expr::Deref { expr } => match expr.node.ty.clone() {
            Type::TyPtr(base) => e.node.ty = *base,
            _ => e.node.ty = Type::TyInt,
        },
    }
}

pub fn sema_stmt(node: &mut ast::StmtWithPos) {
    match &mut node.node {
        ast::Stmt::Block { body } => {
            for stmt in body {
                sema_stmt(stmt);
            }
        }
        ast::Stmt::ExprStmt { expr } => add_type(expr),
        ast::Stmt::Return { expr } => add_type(expr),
        ast::Stmt::NullStmt => (),
        ast::Stmt::IfStmt {
            condition,
            then_clause,
            else_clause,
        } => {
            add_type(condition);
            sema_stmt(then_clause);
            if let Some(els) = else_clause {
                sema_stmt(els);
            }
        }
        ast::Stmt::ForStmt {
            init,
            condition,
            body,
            increment,
        } => {
            sema_stmt(init);
            if let Some(cond) = condition {
                add_type(cond);
            }
            sema_stmt(body);
            if let Some(inc) = increment {
                add_type(inc);
            }
        }
        ast::Stmt::WhileStmt { condition, body } => {
            add_type(condition);
            sema_stmt(body);
        }
    }
}

pub fn sema(program: &mut ast::Program) {
    for n in &mut program.body {
        sema_stmt(n);
    }
}
