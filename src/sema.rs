use std::cell::RefCell;
use std::rc::Rc;

use crate::ast::BinaryOperator::*;
use crate::ast::{self, BinaryOperatorWithPos, Expr, ExprWithPos, UnaryOperatorWithPos};
use crate::position::WithPos;
use crate::token::Token;

#[derive(Clone, Debug, PartialEq)]
pub enum Type {
    TyEnum {
        name: Option<Token>,
    },
    TyBool {
        name: Option<Token>,
    },
    TyVoid {
        name: Option<Token>,
    },
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

impl Type {
    pub fn struct_type(
        name: Option<Token>,
        members: Vec<Rc<RefCell<Member>>>,
        type_size: i32,
        align: i32,
    ) -> Self {
        Type::TyStruct {
            name,
            members,
            type_size,
            align,
        }
    }

    pub fn get_size(&self) -> i32 {
        match self {
            Type::TyEnum { .. } => 4,
            Type::TyBool { .. } => 1,
            Type::TyVoid { .. } => 1,
            Type::TyLong { .. } => 8,
            Type::TyInt { .. } => 4,
            Type::TyShort { .. } => 2,
            Type::TyChar { .. } => 1,
            Type::TyArray {
                name: _,
                base,
                array_len,
            } => base.get_size() * *array_len,
            Type::TyPtr { .. } => 8,
            Type::TyStruct { type_size, .. } => *type_size,
            Type::TyUnion { type_size, .. } => *type_size,
            _ => 0,
        }
    }

    pub fn set_size(&mut self, sz: i32) {
        match self {
            Type::TyStruct { type_size, .. } => *type_size = sz,
            _ => (),
        }
    }

    pub fn get_align(&self) -> i32 {
        match self {
            Type::TyEnum { .. } => 4,
            Type::TyBool { .. } => 1,
            Type::TyVoid { .. } => 1,
            Type::TyStruct { align, .. } => *align,
            Type::TyUnion { align, .. } => *align,
            Type::TyLong { .. } => 8,
            Type::TyInt { .. } => 4,
            Type::TyShort { .. } => 2,
            Type::TyChar { .. } => 1,
            Type::TyArray { base, .. } => base.get_align(),
            Type::TyPtr { .. } => 8,
            _ => panic!("type {:?} has no align infomation.", self),
        }
    }

    pub fn set_align(&mut self, new_align: i32) {
        match self {
            Type::TyStruct { align, .. } => *align = new_align,
            _ => (),
        }
    }

    pub fn is_integer(&self) -> bool {
        match self {
            Type::TyBool { .. }
            | Type::TyChar { .. }
            | Type::TyEnum { .. }
            | Type::TyInt { .. }
            | Type::TyShort { .. }
            | Type::TyLong { .. } => true,
            _ => false,
        }
    }

    pub fn set_name(&mut self, tok: Token) {
        match self {
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

    pub fn set_struct_members(&mut self, new_members: Vec<Rc<RefCell<Member>>>) {
        match self {
            Type::TyStruct {
                ref mut members, ..
            } => *members = new_members,
            _ => (),
        }
    }

    pub fn get_struct_members(&mut self) -> Option<Vec<Rc<RefCell<Member>>>> {
        match self {
            Type::TyStruct { members, .. } => Some(members.clone()),
            _ => None,
        }
    }
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

pub fn get_common_type(ty1: Type, ty2: Type) -> Type {
    match (ty1.clone(), ty2.clone()) {
        (Type::TyArray { base, .. } | Type::TyPtr { base, .. }, _) => pointer_to(*base),
        _ if ty1.get_size() == 8 || ty2.get_size() == 8 => Type::TyLong { name: None },
        _ => Type::TyInt { name: None },
    }
}

/// For many binary operators, we implicitly promote operands so that
/// both operands have the same type. Any integral type smaller than
/// int is always promoted to int. If the type of one operand is larger
/// than the other's (e.g. "long" vs. "int"), the smaller operand will
/// be promoted to match with the other.
///
/// This operation is called the "usual arithmetic conversion".
pub fn usual_arith_conv(lhs: &mut ExprWithPos, rhs: &mut ExprWithPos) {
    let ty = get_common_type(lhs.node.ty.clone(), rhs.node.ty.clone());
    *lhs = WithPos::new(
        WithType::new(
            Expr::CastExpr {
                expr: Box::new(lhs.clone()),
                ty: ty.clone(),
            },
            ty.clone(),
        ),
        lhs.pos,
    );
    *rhs = WithPos::new(
        WithType::new(
            Expr::CastExpr {
                expr: Box::new(rhs.clone()),
                ty: ty.clone(),
            },
            ty.clone(),
        ),
        rhs.pos,
    );
}

pub fn add_type(e: &mut ast::ExprWithPos) {
    match &mut e.node.node {
        ast::Expr::Binary {
            op:
                BinaryOperatorWithPos {
                    node: Eq | Ne | Lt | Le | Ge | Gt,
                    ..
                },
            ref mut left,
            ref mut right,
        } => {
            usual_arith_conv(left, right);
            e.node.ty = Type::TyInt { name: None };
        }
        ast::Expr::Binary {
            op:
                BinaryOperatorWithPos {
                    node: Add | Sub | Mul | Div | Mod | BitAnd | BitOr | BitXor,
                    ..
                },
            ref mut left,
            ref mut right,
        } => {
            usual_arith_conv(left, right);
            e.node.ty = left.node.ty.clone();
        }
        ast::Expr::Binary {
            op:
                BinaryOperatorWithPos {
                    node: LogOr | LogAnd,
                    ..
                },
            ..
        } => e.node.ty = Type::TyInt { name: None },
        ast::Expr::Number { value } => {
            if *value == *value as i32 as i64 {
                e.node.ty = Type::TyInt { name: None };
            } else {
                e.node.ty = Type::TyLong { name: None };
            }
        }
        ast::Expr::Variable { obj } => e.node.ty = obj.borrow().ty.clone(),
        ast::Expr::Assign {
            l_value,
            ref mut r_value,
        } => match l_value.node.ty {
            Type::TyPlaceholder => e.node.ty = l_value.node.ty.clone(),
            Type::TyArray { .. } => panic!("array type is not lvalue. {:?}", l_value.node.ty),
            Type::TyStruct { .. } => e.node.ty = l_value.node.ty.clone(),
            _ => {
                r_value.node.node = Expr::CastExpr {
                    expr: r_value.clone(),
                    ty: l_value.node.ty.clone(),
                };
                e.node.ty = l_value.node.ty.clone();
            }
        },
        ast::Expr::Unary {
            op:
                UnaryOperatorWithPos {
                    node: ast::UnaryOperator::Neg,
                    ..
                },
            expr,
        } => {
            let ty = get_common_type(Type::TyInt { name: None }, expr.node.ty.clone());
            expr.node.node = Expr::CastExpr {
                expr: expr.clone(),
                ty: ty.clone(),
            };
            e.node.ty = ty.clone();
        }
        ast::Expr::Unary {
            op:
                UnaryOperatorWithPos {
                    node: ast::UnaryOperator::Not,
                    ..
                },
            ..
        } => e.node.ty = Type::TyInt { name: None },
        ast::Expr::Unary {
            op:
                UnaryOperatorWithPos {
                    node: ast::UnaryOperator::BitNot,
                    ..
                },
            expr,
        } => e.node.ty = expr.node.ty.clone(),
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
            } => match *base {
                Type::TyVoid { name } => panic!("{:?} dereferencing a void pointer.", name),
                _ => e.node.ty = *base,
            },
            _ => panic!("invalid pointer dereference: {:#?}", expr),
        },
        ast::Expr::FunctionCall { .. } => (),
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
        ast::Expr::CommaExpr { right, .. } => e.node.ty = right.node.ty.clone(),
        ast::Expr::MemberExpr { member, .. } => e.node.ty = member.borrow().ty.clone(),

        ast::Expr::CastExpr { .. } => (),
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

// Round up `n` to the nearest multiple of `align`. For instance,
// align_to(5, 8) returns 8 and align_to(11, 8) returns 16.
pub fn align_to(n: i32, align: i32) -> i32 {
    (n + align - 1) / align * align
}
