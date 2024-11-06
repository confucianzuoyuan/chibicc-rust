use crate::ast::BinaryOperator::*;
use crate::ast::{self, BinaryOperatorWithPos, Expr, ExprWithPos, UnaryOperatorWithPos};
use crate::position::WithPos;
use crate::token::{Tok, Token};

#[derive(Clone, Debug, PartialEq)]
pub enum Ty {
    TyEnum,
    TyBool,
    TyVoid,
    TyShort,
    TyLong,
    TyInt,
    TyChar,
    TyPtr {
        base: Box<Type>,
    },
    TyArray {
        base: Box<Type>,
        array_len: i32,
    },
    TyFunc {
        params: Vec<Type>,
        return_ty: Box<Type>,
    },
    TyStruct {
        members: Vec<Member>,
        type_size: i32,
        align: i32,
    },
    TyUnion {
        members: Vec<Member>,
        type_size: i32,
        align: i32,
    },
    TyPlaceholder,
}

#[derive(Clone, Debug, PartialEq)]
pub struct Type {
    pub ty: Ty,
    pub name: Option<Token>,
}

impl Type {
    pub fn new_placeholder() -> Self {
        Type {
            ty: Ty::TyPlaceholder,
            name: None,
        }
    }

    pub fn new_void() -> Self {
        Type {
            ty: Ty::TyVoid,
            name: None,
        }
    }

    pub fn new_bool() -> Self {
        Type {
            ty: Ty::TyBool,
            name: None,
        }
    }

    pub fn new_char() -> Self {
        Type {
            ty: Ty::TyChar,
            name: None,
        }
    }

    pub fn new_short() -> Self {
        Type {
            ty: Ty::TyShort,
            name: None,
        }
    }

    pub fn new_int() -> Self {
        Type {
            ty: Ty::TyInt,
            name: None,
        }
    }

    pub fn new_enum() -> Self {
        Type {
            ty: Ty::TyEnum,
            name: None,
        }
    }

    pub fn new_long() -> Self {
        Type {
            ty: Ty::TyLong,
            name: None,
        }
    }

    pub fn new_union(members: Vec<Member>, type_size: i32, align: i32) -> Self {
        Type {
            ty: Ty::TyUnion {
                members,
                type_size,
                align,
            },
            name: None,
        }
    }

    pub fn new_func(params: Vec<Type>, return_ty: Type) -> Self {
        Type {
            ty: Ty::TyFunc {
                params,
                return_ty: Box::new(return_ty),
            },
            name: None,
        }
    }

    pub fn new_ptr(base: Type) -> Self {
        Type {
            ty: Ty::TyPtr {
                base: Box::new(base),
            },
            name: None,
        }
    }

    pub fn new_array(base: Type, array_len: i32) -> Self {
        Type {
            ty: Ty::TyArray {
                base: Box::new(base),
                array_len,
            },
            name: None,
        }
    }

    pub fn get_token(&self) -> Option<Token> {
        self.name.clone()
    }

    pub fn get_ident(&self) -> Option<String> {
        match &self.name {
            Some(t) => match t.token.clone() {
                Tok::Ident(name) => Some(name),
                _ => None,
            },
            _ => None,
        }
    }

    pub fn struct_type(
        name: Option<Token>,
        members: Vec<Member>,
        type_size: i32,
        align: i32,
    ) -> Self {
        Self {
            ty: Ty::TyStruct {
                members,
                type_size,
                align,
            },
            name,
        }
    }

    pub fn array_type(base: Type, len: i32) -> Self {
        Self {
            ty: Ty::TyArray {
                base: Box::new(base),
                array_len: len,
            },
            name: None,
        }
    }

    pub fn base(&self) -> Option<Type> {
        match &self.ty {
            Ty::TyArray { base, .. } => Some(*base.clone()),
            _ => None,
        }
    }

    pub fn is_void(&self) -> bool {
        match self.ty {
            Ty::TyVoid => true,
            _ => false,
        }
    }

    pub fn is_enum(&self) -> bool {
        match self.ty {
            Ty::TyEnum => true,
            _ => false,
        }
    }

    pub fn is_array(&self) -> bool {
        match self.ty {
            Ty::TyArray { .. } => true,
            _ => false,
        }
    }

    pub fn is_func(&self) -> bool {
        match self.ty {
            Ty::TyFunc { .. } => true,
            _ => false,
        }
    }

    pub fn is_struct(&self) -> bool {
        match self.ty {
            Ty::TyStruct { .. } => true,
            _ => false,
        }
    }

    pub fn is_union(&self) -> bool {
        match self.ty {
            Ty::TyUnion { .. } => true,
            _ => false,
        }
    }

    pub fn get_array_len(&self) -> i32 {
        match &self.ty {
            Ty::TyArray { array_len, .. } => *array_len,
            _ => 0,
        }
    }

    pub fn get_size(&self) -> i32 {
        match &self.ty {
            Ty::TyBool | Ty::TyChar | Ty::TyVoid => 1,
            Ty::TyShort => 2,
            Ty::TyInt | Ty::TyEnum => 4,
            Ty::TyLong | Ty::TyPtr { .. } => 8,
            Ty::TyArray { base, array_len } => base.get_size() * *array_len,
            Ty::TyStruct { type_size, .. } => *type_size,
            Ty::TyUnion { type_size, .. } => *type_size,
            _ => 0,
        }
    }

    pub fn set_size(&mut self, sz: i32) {
        match self.ty {
            Ty::TyStruct {
                ref mut type_size, ..
            } => *type_size = sz,
            _ => (),
        }
    }

    pub fn get_align(&self) -> i32 {
        match &self.ty {
            Ty::TyChar | Ty::TyBool | Ty::TyVoid => 1,
            Ty::TyShort => 2,
            Ty::TyInt | Ty::TyEnum => 4,
            Ty::TyLong | Ty::TyPtr { .. } => 8,
            Ty::TyStruct { align, .. } | Ty::TyUnion { align, .. } => *align,
            Ty::TyArray { base, .. } => base.get_align(),
            _ => panic!("type {:?} has no align infomation.", self),
        }
    }

    pub fn set_align(&mut self, new_align: i32) {
        match self.ty {
            Ty::TyStruct { ref mut align, .. } => *align = new_align,
            _ => (),
        }
    }

    pub fn is_integer(&self) -> bool {
        match self.ty {
            Ty::TyBool | Ty::TyChar | Ty::TyEnum | Ty::TyInt | Ty::TyShort | Ty::TyLong => true,
            _ => false,
        }
    }

    pub fn set_name(&mut self, tok: Token) {
        self.name = Some(tok.clone());
    }

    pub fn set_struct_members(&mut self, new_members: Vec<Member>) {
        match self.ty {
            Ty::TyStruct {
                ref mut members, ..
            } => *members = new_members,
            _ => (),
        }
    }

    pub fn get_struct_members(&mut self) -> Option<Vec<Member>> {
        match &self.ty {
            Ty::TyStruct { members, .. } => Some(members.clone()),
            _ => None,
        }
    }

    pub fn get_union_members(&mut self) -> Option<Vec<Member>> {
        match &self.ty {
            Ty::TyUnion { members, .. } => Some(members.clone()),
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
    Type {
        ty: Ty::TyPtr {
            base: Box::new(base),
        },
        name: None,
    }
}

pub fn get_common_type(ty1: Type, ty2: Type) -> Type {
    match (ty1.clone().ty, ty2.clone().ty) {
        (Ty::TyArray { base, .. } | Ty::TyPtr { base, .. }, _) => pointer_to(*base),
        _ if ty1.get_size() == 8 || ty2.get_size() == 8 => Type {
            ty: Ty::TyLong,
            name: None,
        },
        _ => Type {
            ty: Ty::TyInt,
            name: None,
        },
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
            e.node.ty = Type {
                ty: Ty::TyInt,
                name: None,
            };
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
        } => {
            e.node.ty = Type {
                ty: Ty::TyInt,
                name: None,
            }
        }
        ast::Expr::Number { value } => {
            if *value == *value as i32 as i64 {
                e.node.ty = Type {
                    ty: Ty::TyInt,
                    name: None,
                };
            } else {
                e.node.ty = Type {
                    ty: Ty::TyLong,
                    name: None,
                };
            }
        }
        ast::Expr::Variable { obj } => e.node.ty = obj.borrow().ty.clone(),
        ast::Expr::Assign {
            l_value,
            ref mut r_value,
        } => match l_value.node.ty.ty {
            Ty::TyPlaceholder => e.node.ty = l_value.node.ty.clone(),
            Ty::TyArray { .. } => panic!("array type is not lvalue. {:?}", l_value.node.ty),
            Ty::TyStruct { .. } => e.node.ty = l_value.node.ty.clone(),
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
            let ty = get_common_type(
                Type {
                    ty: Ty::TyInt,
                    name: None,
                },
                expr.node.ty.clone(),
            );
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
        } => {
            e.node.ty = Type {
                ty: Ty::TyInt,
                name: None,
            }
        }
        ast::Expr::Unary {
            op:
                UnaryOperatorWithPos {
                    node: ast::UnaryOperator::BitNot,
                    ..
                },
            expr,
        } => e.node.ty = expr.node.ty.clone(),
        ast::Expr::Binary {
            op: BinaryOperatorWithPos {
                node: SHL | SHR, ..
            },
            left,
            ..
        } => e.node.ty = left.node.ty.clone(),
        // "&" addr
        ast::Expr::Addr { expr } => match expr.node.ty.clone().ty {
            Ty::TyArray { base, array_len: _ } => e.node.ty = pointer_to(*base),
            _ => e.node.ty = pointer_to(expr.node.ty.clone()),
        },
        // "*" dereference
        ast::Expr::Deref { expr } => match expr.node.ty.clone().ty {
            Ty::TyPtr { base, .. } => e.node.ty = *base,
            Ty::TyArray { base, array_len: _ } => match base.ty {
                Ty::TyVoid => panic!("{:?} dereferencing a void pointer.", base.name),
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
        ast::Expr::MemberExpr { member, .. } => e.node.ty = member.ty.clone(),

        ast::Expr::CastExpr { .. } => (),
        ast::Expr::Null => (),
        ast::Expr::TernaryExpr {
            condition: _,
            ref mut then_clause,
            ref mut else_clause,
        } => {
            if then_clause.node.ty.is_void() || else_clause.node.ty.is_void() {
                e.node.ty = Type {
                    ty: Ty::TyVoid,
                    name: None,
                }
            } else {
                usual_arith_conv(then_clause, else_clause);
                e.node.ty = then_clause.node.ty.clone();
            }
        }
        ast::Expr::MemZero { .. } => (),
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
            break_label: _,
            continue_label: _,
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
        ast::Stmt::WhileStmt {
            condition, body, ..
        } => {
            add_type(condition);
            sema_stmt(body);
        }
        ast::Stmt::LabelStmt { label: _, stmt } => sema_stmt(stmt),
        _ => (),
    }
}

// Round up `n` to the nearest multiple of `align`. For instance,
// align_to(5, 8) returns 8 and align_to(11, 8) returns 16.
pub fn align_to(n: i32, align: i32) -> i32 {
    (n + align - 1) / align * align
}
