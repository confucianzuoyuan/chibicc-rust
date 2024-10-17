use std::cell::RefCell;
use std::rc::Rc;

use crate::ast::BinaryOperator::{Add, Div, Eq, Ge, Gt, Le, Lt, Mul, Ne, Sub};
use crate::ast::{self, BinaryOperatorWithPos, UnaryOperatorWithPos};
use crate::token::Token;

#[derive(Clone, Debug, PartialEq)]
pub enum Type {
    TyShort {
        name: Option<Token>,
    },
    TyLong {
        name: Option<Token>,
    },
    TyInt {
        name: Option<Token>,
    },
    TyChar {
        name: Option<Token>,
    },
    TyPtr {
        base: Box<Type>,
        name: Option<Token>,
    },
    TyArray {
        name: Option<Token>,
        base: Box<Type>,
        array_len: i32,
    },
    TyFunc {
        name: Option<Token>,
        params: Vec<Type>,
        return_ty: Box<Type>,
    },
    TyStruct {
        name: Option<Token>,
        members: Vec<Rc<RefCell<Member>>>,
        type_size: i32,
        align: i32,
    },
    TyUnion {
        name: Option<Token>,
        members: Vec<Rc<RefCell<Member>>>,
        type_size: i32,
        align: i32,
    },
    TyPlaceholder,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Member {
    pub ty: Type,
    pub name: Token,
    pub offset: i32,
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

pub fn pointer_to(base: Type) -> Type {
    Type::TyPtr {
        base: Box::new(base),
        name: None,
    }
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
        | ast::Expr::Number { .. } => {
            if e.node.ty == Type::TyPlaceholder {
                e.node.ty = Type::TyInt { name: None };
            }
        }
        ast::Expr::Variable { obj } => {
            if e.node.ty == Type::TyPlaceholder {
                e.node.ty = obj.borrow().ty.clone();
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
        ast::Expr::Assign { l_value, .. } => match e.node.ty {
            Type::TyPlaceholder => e.node.ty = l_value.node.ty.clone(),
            Type::TyArray { .. } => panic!("array type is not lvalue."),
            _ => (),
        },
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
        // "&" addr
        ast::Expr::Addr { expr } => match e.node.ty.clone() {
            Type::TyPlaceholder => e.node.ty = pointer_to(expr.node.ty.clone()),
            Type::TyArray {
                name: _,
                base,
                array_len: _,
            } => e.node.ty = pointer_to(*base),
            _ => (),
        },
        // "*" dereference
        ast::Expr::Deref { expr } => match expr.node.ty.clone() {
            Type::TyPtr { base, .. } => e.node.ty = *base,
            Type::TyArray {
                name: _,
                base,
                array_len: _,
            } => e.node.ty = *base,
            _ => panic!("invalid pointer dereference: {:#?}", expr),
        },
        ast::Expr::FunctionCall { .. } => {
            if e.node.ty == Type::TyPlaceholder {
                e.node.ty = Type::TyInt { name: None }
            }
        }
        ast::Expr::StmtExpr { body } => {
            if body.len() > 0 {
                let stmt = body.last().unwrap().node.clone();
                match stmt {
                    ast::Stmt::ExprStmt { expr } => e.node.ty = expr.node.ty,
                    _ => panic!("statement expression returning void is not supported."),
                }
            } else {
                panic!("statement expression returning void is not supported.");
            }
        }
        ast::Expr::CommaExpr { right, .. } => {
            if e.node.ty == Type::TyPlaceholder {
                e.node.ty = right.node.ty.clone();
            }
        }
        ast::Expr::MemberExpr { member, .. } => {
            if e.node.ty == Type::TyPlaceholder {
                e.node.ty = member.borrow().ty.clone();
            }
        }
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

pub fn get_sizeof(ty: Type) -> i32 {
    match ty {
        Type::TyLong { .. } => 8,
        Type::TyInt { .. } => 4,
        Type::TyShort { .. } => 2,
        Type::TyChar { .. } => 1,
        Type::TyArray {
            name: _,
            base,
            array_len,
        } => get_sizeof(*base) * array_len,
        Type::TyPtr { .. } => 8,
        Type::TyStruct { type_size, .. } => type_size,
        Type::TyUnion { type_size, .. } => type_size,
        _ => 0,
    }
}

// Round up `n` to the nearest multiple of `align`. For instance,
// align_to(5, 8) returns 8 and align_to(11, 8) returns 16.
pub fn align_to(n: i32, align: i32) -> i32 {
    (n + align - 1) / align * align
}

pub fn get_align(ty: Type) -> i32 {
    match ty {
        Type::TyStruct { align, .. } => align,
        Type::TyUnion { align, .. } => align,
        Type::TyLong { .. } => 8,
        Type::TyInt { .. } => 4,
        Type::TyShort { .. } => 2,
        Type::TyChar { .. } => 1,
        Type::TyArray { base, .. } => get_align(*base),
        Type::TyPtr { .. } => 8,
        _ => panic!("type {:?} has no align infomation.", ty),
    }
}
