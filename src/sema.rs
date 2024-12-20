use crate::ast::BinaryOperator::*;
use crate::ast::{self, BinaryOperatorWithPos, Expr, ExprInner, UnaryOperatorWithPos};
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
    TyUShort,
    TyULong,
    TyUInt,
    TyUChar,
    TyFloat,
    TyDouble,
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
        is_variadic: bool,
    },
    TyStruct {
        members: Vec<Member>,
        type_size: i32,
        align: i32,
        is_flexible: bool,
    },
    TyUnion {
        members: Vec<Member>,
        type_size: i32,
        align: i32,
        is_flexible: bool,
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

    pub fn new_float() -> Self {
        Type {
            ty: Ty::TyFloat,
            name: None,
        }
    }

    pub fn new_double() -> Self {
        Type {
            ty: Ty::TyDouble,
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

    pub fn new_uchar() -> Self {
        Type {
            ty: Ty::TyUChar,
            name: None,
        }
    }

    pub fn new_short() -> Self {
        Type {
            ty: Ty::TyShort,
            name: None,
        }
    }

    pub fn new_ushort() -> Self {
        Type {
            ty: Ty::TyUShort,
            name: None,
        }
    }

    pub fn new_int() -> Self {
        Type {
            ty: Ty::TyInt,
            name: None,
        }
    }

    pub fn new_uint() -> Self {
        Type {
            ty: Ty::TyUInt,
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

    pub fn new_ulong() -> Self {
        Type {
            ty: Ty::TyULong,
            name: None,
        }
    }

    pub fn new_func(params: Vec<Type>, return_ty: Type, is_variadic: bool) -> Self {
        Type {
            ty: Ty::TyFunc {
                params,
                return_ty: Box::new(return_ty),
                is_variadic,
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
                is_flexible: false,
            },
            name,
        }
    }

    pub fn union_type(
        name: Option<Token>,
        members: Vec<Member>,
        type_size: i32,
        align: i32,
    ) -> Self {
        Type {
            ty: Ty::TyUnion {
                members,
                type_size,
                align,
                is_flexible: false,
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
            Ty::TyArray { base, .. } | Ty::TyPtr { base, .. } => Some(*base.clone()),
            _ => None,
        }
    }

    pub fn is_variadic(&self) -> bool {
        match self.ty {
            Ty::TyFunc { is_variadic, .. } => is_variadic,
            _ => false,
        }
    }

    pub fn is_flexible(&self) -> bool {
        match self.ty {
            Ty::TyStruct { is_flexible, .. } | Ty::TyUnion { is_flexible, .. } => is_flexible,
            _ => false,
        }
    }

    pub fn set_flexible(&mut self, v: bool) {
        match self.ty {
            Ty::TyStruct {
                ref mut is_flexible,
                ..
            }
            | Ty::TyUnion {
                ref mut is_flexible,
                ..
            } => *is_flexible = v,
            _ => (),
        }
    }

    pub fn set_last_member_type(&mut self, ty: Type) {
        match self.ty {
            Ty::TyStruct {
                ref mut members, ..
            }
            | Ty::TyUnion {
                ref mut members, ..
            } => members.last_mut().unwrap().ty = ty,
            _ => (),
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

    pub fn is_ptr(&self) -> bool {
        match self.ty {
            Ty::TyPtr { .. } => true,
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

    pub fn is_flonum(&self) -> bool {
        match self.ty {
            Ty::TyFloat | Ty::TyDouble => true,
            _ => false,
        }
    }

    pub fn is_float(&self) -> bool {
        match self.ty {
            Ty::TyFloat => true,
            _ => false,
        }
    }

    pub fn is_double(&self) -> bool {
        match self.ty {
            Ty::TyDouble => true,
            _ => false,
        }
    }

    pub fn is_number(&self) -> bool {
        self.is_integer() || self.is_flonum()
    }

    pub fn get_array_len(&self) -> i32 {
        match &self.ty {
            Ty::TyArray { array_len, .. } => *array_len,
            _ => 0,
        }
    }

    pub fn set_array_len(&mut self, sz: i32) {
        match self.ty {
            Ty::TyArray {
                ref mut array_len, ..
            } => *array_len = sz,
            _ => (),
        }
    }

    pub fn is_unsigned(&self) -> bool {
        match self.ty {
            Ty::TyUChar | Ty::TyUShort | Ty::TyUInt | Ty::TyULong | Ty::TyPtr { .. } => true,
            _ => false,
        }
    }

    pub fn get_size(&self) -> i32 {
        match &self.ty {
            Ty::TyBool | Ty::TyChar | Ty::TyVoid | Ty::TyUChar => 1,
            Ty::TyShort | Ty::TyUShort => 2,
            Ty::TyInt | Ty::TyUInt | Ty::TyEnum | Ty::TyFloat => 4,
            Ty::TyLong | Ty::TyULong | Ty::TyDouble | Ty::TyPtr { .. } => 8,
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
            Ty::TyChar | Ty::TyUChar | Ty::TyBool | Ty::TyVoid => 1,
            Ty::TyShort | Ty::TyUShort => 2,
            Ty::TyInt | Ty::TyUInt | Ty::TyEnum | Ty::TyFloat => 4,
            Ty::TyLong | Ty::TyULong | Ty::TyDouble | Ty::TyPtr { .. } => 8,
            Ty::TyStruct { align, .. } | Ty::TyUnion { align, .. } => *align,
            Ty::TyArray { base, .. } => base.get_align(),
            _ => 0,
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
            Ty::TyBool
            | Ty::TyChar
            | Ty::TyEnum
            | Ty::TyInt
            | Ty::TyShort
            | Ty::TyLong
            | Ty::TyUChar
            | Ty::TyUInt
            | Ty::TyUShort
            | Ty::TyULong => true,
            _ => false,
        }
    }

    pub fn set_members(&mut self, new_members: Vec<Member>) {
        match self.ty {
            Ty::TyStruct {
                ref mut members, ..
            }
            | Ty::TyUnion {
                ref mut members, ..
            } => *members = new_members,
            _ => (),
        }
    }

    pub fn get_members(&mut self) -> Vec<Member> {
        match &self.ty {
            Ty::TyStruct { members, .. } | Ty::TyUnion { members, .. } => members.clone(),
            _ => vec![],
        }
    }

    pub fn get_member(&mut self, name: String) -> Option<Member> {
        match &self.ty {
            Ty::TyStruct { members, .. } | Ty::TyUnion { members, .. } => {
                for member in members {
                    if member.name.token.get_ident_name().unwrap() == name {
                        return Some(member.clone());
                    }
                }
                None
            }
            _ => None,
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Member {
    pub ty: Type,
    pub name: Token,
    pub offset: i32,
    pub align: i32,
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
        (Ty::TyDouble, _) | (_, Ty::TyDouble) => Type::new_double(),
        (Ty::TyFloat, _) | (_, Ty::TyFloat) => Type::new_float(),
        (Ty::TyFunc { .. }, _) => pointer_to(ty1),
        (_, Ty::TyFunc { .. }) => pointer_to(ty2),
        _ => {
            let ty1 = if ty1.get_size() < 4 {
                Type::new_int()
            } else {
                ty1.clone()
            };
            let ty2 = if ty2.get_size() < 4 {
                Type::new_int()
            } else {
                ty2.clone()
            };
            if ty1.get_size() > ty2.get_size() {
                return ty1;
            } else if ty1.get_size() < ty2.get_size() {
                return ty2;
            } else {
                if ty2.is_unsigned() {
                    return ty2;
                } else {
                    return ty1;
                }
            }
        }
    }
}

/// For many binary operators, we implicitly promote operands so that
/// both operands have the same type. Any integral type smaller than
/// int is always promoted to int. If the type of one operand is larger
/// than the other's (e.g. "long" vs. "int"), the smaller operand will
/// be promoted to match with the other.
///
/// This operation is called the "usual arithmetic conversion".
pub fn usual_arith_conv(lhs: &mut Expr, rhs: &mut Expr) {
    let ty = get_common_type(lhs.ty.clone(), rhs.ty.clone());
    *lhs = Expr::new_cast_expr(lhs.clone(), ty.clone(), lhs.pos);
    *rhs = Expr::new_cast_expr(rhs.clone(), ty, rhs.pos);
}

pub fn add_type(e: &mut ast::Expr) {
    match &mut e.expr {
        ExprInner::Binary {
            op:
                BinaryOperatorWithPos {
                    node: Eq | Ne | Lt | Le | Ge | Gt,
                    ..
                },
            ref mut left,
            ref mut right,
        } => {
            usual_arith_conv(left, right);
            e.ty = Type::new_int();
        }
        ExprInner::Binary {
            op:
                BinaryOperatorWithPos {
                    node: Add | Sub | Mul | Div | Mod | BitAnd | BitOr | BitXor,
                    ..
                },
            ref mut left,
            ref mut right,
        } => {
            usual_arith_conv(left, right);
            e.ty = left.ty.clone();
        }
        ExprInner::Binary {
            op:
                BinaryOperatorWithPos {
                    node: LogOr | LogAnd,
                    ..
                },
            ..
        } => e.ty = Type::new_int(),
        ExprInner::ConstInt { .. } => e.ty = Type::new_int(),
        ExprInner::ConstUInt { .. } => e.ty = Type::new_uint(),
        ExprInner::ConstLong { .. } => e.ty = Type::new_long(),
        ExprInner::ConstULong { .. } => e.ty = Type::new_ulong(),
        ExprInner::ConstFloat { .. } => e.ty = Type::new_float(),
        ExprInner::ConstDouble { .. } => e.ty = Type::new_double(),
        ExprInner::Variable { ref obj } => e.ty = obj.borrow().ty.clone(),
        ExprInner::Assign {
            l_value,
            ref mut r_value,
        } => match l_value.ty.ty {
            Ty::TyPlaceholder => e.ty = l_value.ty.clone(),
            Ty::TyArray { .. } => panic!("array type is not lvalue. {:?}", l_value.ty),
            Ty::TyStruct { .. } => e.ty = l_value.ty.clone(),
            _ => {
                r_value.expr = ExprInner::CastExpr {
                    expr: r_value.clone(),
                    ty: l_value.ty.clone(),
                };
                e.ty = l_value.ty.clone();
            }
        },
        ExprInner::Unary {
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
                expr.ty.clone(),
            );
            expr.expr = ExprInner::CastExpr {
                expr: expr.clone(),
                ty: ty.clone(),
            };
            e.ty = ty.clone();
        }
        ExprInner::Unary {
            op:
                UnaryOperatorWithPos {
                    node: ast::UnaryOperator::Not,
                    ..
                },
            ..
        } => {
            e.ty = Type {
                ty: Ty::TyInt,
                name: None,
            }
        }
        ExprInner::Unary {
            op:
                UnaryOperatorWithPos {
                    node: ast::UnaryOperator::BitNot,
                    ..
                },
            expr,
        } => e.ty = expr.ty.clone(),
        ExprInner::Binary {
            op: BinaryOperatorWithPos {
                node: SHL | SHR, ..
            },
            left,
            ..
        } => e.ty = left.ty.clone(),
        // "&" addr
        ExprInner::Addr { expr } => match expr.ty.clone().ty {
            Ty::TyArray { base, array_len: _ } => e.ty = pointer_to(*base),
            _ => e.ty = pointer_to(expr.ty.clone()),
        },
        // "*" dereference
        ExprInner::Deref { expr } => match expr.ty.clone().ty {
            Ty::TyPtr { base, .. } => e.ty = *base,
            Ty::TyArray { base, array_len: _ } => match base.ty {
                Ty::TyVoid => panic!("{:?} dereferencing a void pointer.", base.name),
                _ => e.ty = *base,
            },
            _ => panic!(
                "invalid pointer dereference: {:?}\n{:?}",
                expr.ty, expr.expr
            ),
        },
        ExprInner::FunctionCall { .. } => (),
        ExprInner::StmtExpr { body } => {
            if body.len() > 0 {
                let stmt = body.last().unwrap().node.clone();
                match stmt {
                    ast::Stmt::ExprStmt { expr } => e.ty = expr.ty,
                    _ => panic!("statement expression returning void is not supported."),
                }
            } else {
                panic!("statement expression returning void is not supported.");
            }
        }
        ExprInner::CommaExpr { right, .. } => e.ty = right.ty.clone(),
        ExprInner::MemberExpr { member, .. } => e.ty = member.ty.clone(),

        ExprInner::CastExpr { .. } => (),
        ExprInner::Null => (),
        ExprInner::TernaryExpr {
            condition: _,
            ref mut then_clause,
            ref mut else_clause,
        } => {
            if then_clause.ty.is_void() || else_clause.ty.is_void() {
                e.ty = Type {
                    ty: Ty::TyVoid,
                    name: None,
                }
            } else {
                usual_arith_conv(then_clause, else_clause);
                e.ty = then_clause.ty.clone();
            }
        }
        ExprInner::MemZero { .. } => (),
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
        ast::Stmt::Return { expr: Some(_expr) } => add_type(_expr),
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
