use std::{collections::HashMap, io::Write};

use crate::{
    ast::{self, FunctionDefinition, InitData},
    sema::{self, Ty, Type},
};

static ARGREG_64: [&str; 6] = ["%rdi", "%rsi", "%rdx", "%rcx", "%r8", "%r9"];
static ARGREG_32: [&str; 6] = ["%edi", "%esi", "%edx", "%ecx", "%r8d", "%r9d"];
static ARGREG_16: [&str; 6] = ["%di", "%si", "%dx", "%cx", "%r8w", "%r9w"];
static ARGREG_8: [&str; 6] = ["%dil", "%sil", "%dl", "%cl", "%r8b", "%r9b"];

/// The table for type casts
static I32_TO_I8: &str = "movsbl %al, %eax";
static I32_TO_U8: &str = "movzbl %al, %eax";
static I32_TO_I16: &str = "movswl %ax, %eax";
static I32_TO_U16: &str = "movzwl %ax, %eax";
static I32_TO_I64: &str = "movsxd %eax, %rax";
static U32_TO_I64: &str = "mov %eax, %eax";

static CAST_TABLE: [[Option<&str>; 8]; 8] = [
    [
        None,
        None,
        None,
        Some(I32_TO_I64),
        Some(I32_TO_U8),
        Some(I32_TO_U16),
        None,
        Some(I32_TO_I64),
    ], // i8
    [
        Some(I32_TO_I8),
        None,
        None,
        Some(I32_TO_I64),
        Some(I32_TO_U8),
        Some(I32_TO_U16),
        None,
        Some(I32_TO_I64),
    ], // i16
    [
        Some(I32_TO_I8),
        Some(I32_TO_I16),
        None,
        Some(I32_TO_I64),
        Some(I32_TO_U8),
        Some(I32_TO_U16),
        None,
        Some(I32_TO_I64),
    ], // i32
    [
        Some(I32_TO_I8),
        Some(I32_TO_I16),
        None,
        None,
        Some(I32_TO_U8),
        Some(I32_TO_U16),
        None,
        None,
    ], // i64
    [
        Some(I32_TO_I8),
        None,
        None,
        Some(I32_TO_I64),
        None,
        None,
        None,
        Some(I32_TO_I64),
    ], // u8
    [
        Some(I32_TO_I8),
        Some(I32_TO_I16),
        None,
        Some(I32_TO_I64),
        Some(I32_TO_U8),
        None,
        None,
        Some(I32_TO_I64),
    ], // u16
    [
        Some(I32_TO_I8),
        Some(I32_TO_I16),
        None,
        Some(U32_TO_I64),
        Some(I32_TO_U8),
        Some(I32_TO_U16),
        None,
        Some(U32_TO_I64),
    ], // u32
    [
        Some(I32_TO_I8),
        Some(I32_TO_I16),
        None,
        None,
        Some(I32_TO_U8),
        Some(I32_TO_U16),
        None,
        None,
    ], // u64
];

#[derive(Debug)]
pub enum TypeId {
    I8,
    I16,
    I32,
    I64,
    U8,
    U16,
    U32,
    U64,
}

pub fn typeid_to_usize(t: TypeId) -> usize {
    match t {
        TypeId::I8 => 0,
        TypeId::I16 => 1,
        TypeId::I32 => 2,
        TypeId::I64 => 3,
        TypeId::U8 => 4,
        TypeId::U16 => 5,
        TypeId::U32 => 6,
        TypeId::U64 => 7,
    }
}

pub fn get_type_id(ty: Type) -> TypeId {
    match ty.ty {
        Ty::TyChar => TypeId::I8,
        Ty::TyShort => TypeId::I16,
        Ty::TyInt => TypeId::I32,
        Ty::TyLong => TypeId::I64,
        Ty::TyUChar => TypeId::U8,
        Ty::TyUShort => TypeId::U16,
        Ty::TyUInt => TypeId::U32,
        Ty::TyULong => TypeId::U64,
        _ => TypeId::U64,
    }
}

pub struct CodeGenerator {
    depth: u32,
    label_count: u32,
    current_fn: Option<FunctionDefinition>,
    output: Vec<String>,
    out_writer: Box<dyn Write>,

    file_path: String,

    goto_labels: HashMap<String, String>,
}

impl CodeGenerator {
    pub fn new(out_writer: Box<dyn Write>, file_path: String) -> Self {
        CodeGenerator {
            depth: 0,
            label_count: 1,
            current_fn: None,
            output: Vec::new(),
            out_writer,

            file_path,

            goto_labels: HashMap::new(),
        }
    }

    fn push(&mut self) {
        self.output.push(format!("  push %rax"));
        self.depth += 1;
    }

    fn pop(&mut self, arg: String) {
        self.output.push(format!("  pop {}", arg));
        self.depth -= 1;
    }

    // Load a value from where %rax is pointing to.
    fn load(&mut self, ty: &Type) {
        match ty.ty {
            // If it is an array, do not attempt to load a value to the
            // register because in general we can't load an entire array to a
            // register. As a result, the result of an evaluation of an array
            // becomes not the array itself but the address of the array.
            // This is where "array is automatically converted to a pointer to
            // the first element of the array in C" occurs.
            Ty::TyArray { .. } => (),
            Ty::TyStruct { .. } => (),
            Ty::TyUnion { .. } => (),
            // When we load a char or a short value to a register, we always
            // extend them to the size of int, so we can assume the lower half of
            // a register always contains a valid value. The upper half of a
            // register for char, short and int may contain garbage. When we load
            // a long value to a register, it simply occupies the entire register.
            _ if ty.is_unsigned() => match ty.get_size() {
                1 => self.output.push(format!("  movzbl (%rax), %eax")),
                2 => self.output.push(format!("  movzwl (%rax), %eax")),
                4 => self.output.push(format!("  movsxd (%rax), %rax")),
                _ => self.output.push(format!("  mov (%rax), %rax")),
            },
            _ => match ty.get_size() {
                1 => self.output.push(format!("  movsbl (%rax), %eax")),
                2 => self.output.push(format!("  movswl (%rax), %eax")),
                4 => self.output.push(format!("  movsxd (%rax), %rax")),
                _ => self.output.push(format!("  mov (%rax), %rax")),
            },
        }
    }

    // Store %rax to an address that the stack top is pointing to.
    fn store(&mut self, ty: &Type) {
        self.pop("%rdi".to_string());
        match ty.ty {
            Ty::TyStruct { type_size, .. } | Ty::TyUnion { type_size, .. } => {
                for i in 0..=type_size - 1 {
                    self.output.push(format!("  mov {}(%rax), %r8b", i));
                    self.output.push(format!("  mov %r8b, {}(%rdi)", i));
                }
            }
            _ => match ty.get_size() {
                1 => self.output.push(format!("  mov %al, (%rdi)")),
                2 => self.output.push(format!("  mov %ax, (%rdi)")),
                4 => self.output.push(format!("  mov %eax, (%rdi)")),
                _ => self.output.push(format!("  mov %rax, (%rdi)")),
            },
        }
    }

    fn store_gp(&mut self, r: i32, offset: i32, sz: i32) {
        match sz {
            1 => self
                .output
                .push(format!("  mov {}, {}(%rbp)", ARGREG_8[r as usize], offset)),
            2 => self
                .output
                .push(format!("  mov {}, {}(%rbp)", ARGREG_16[r as usize], offset)),
            4 => self
                .output
                .push(format!("  mov {}, {}(%rbp)", ARGREG_32[r as usize], offset)),
            8 => self
                .output
                .push(format!("  mov {}, {}(%rbp)", ARGREG_64[r as usize], offset)),
            _ => unreachable!(),
        }
    }

    fn cast(&mut self, from: Type, to: Type) {
        match to.ty {
            Ty::TyVoid { .. } => (),
            Ty::TyBool { .. } => {
                self.cmp_zero(from);
                self.output.push(format!("  setne %al"));
                self.output.push(format!("  movzx %al, %eax"));
            }
            _ => {
                let t1 = get_type_id(from.clone());
                let t2 = get_type_id(to.clone());
                let t1 = typeid_to_usize(t1);
                let t2 = typeid_to_usize(t2);
                if let Some(_cast) = CAST_TABLE[t1][t2] {
                    self.output.push(format!("  {}", _cast));
                }
            }
        }
    }

    fn cmp_zero(&mut self, ty: Type) {
        if ty.is_integer() && ty.get_size() <= 4 {
            self.output.push(format!("  cmp $0, %eax"));
        } else {
            self.output.push(format!("  cmp $0, %rax"));
        }
    }

    fn gen_addr(&mut self, ast: &ast::ExprWithPos) {
        match &ast.node.node {
            ast::Expr::Variable { obj, .. } => {
                if obj.borrow().is_local {
                    self.output
                        .push(format!("  lea {}(%rbp), %rax", obj.borrow().offset));
                } else {
                    self.output.push(format!(
                        "  lea {}(%rip), %rax",
                        obj.borrow().name.clone().unwrap()
                    ));
                }
            }
            ast::Expr::Deref { expr, .. } => self.gen_expr(expr),
            ast::Expr::CommaExpr { left, right } => {
                self.gen_expr(&left);
                self.gen_addr(&right);
            }
            ast::Expr::MemberExpr { strct, member } => {
                self.gen_addr(&strct);
                self.output.push(format!("  add ${}, %rax", member.offset));
            }
            _ => panic!("not an lvalue"),
        }
    }

    fn gen_stmt(&mut self, ast: &ast::StmtWithPos) {
        self.output.push(format!("  .loc 1 {}", ast.pos.line));
        match &ast.node {
            ast::Stmt::ExprStmt { expr } => self.gen_expr(expr),
            ast::Stmt::Return { expr } => {
                if let Some(expr) = expr {
                    self.gen_expr(expr);
                }
                self.output.push(format!(
                    "  jmp .L.return.{}",
                    self.current_fn.as_ref().unwrap().name.clone().unwrap()
                ));
            }
            ast::Stmt::Block { body } => {
                for n in body {
                    self.gen_stmt(n);
                }
            }
            ast::Stmt::NullStmt => (),
            ast::Stmt::IfStmt {
                condition,
                then_clause,
                else_clause,
            } => {
                let c = self.label_count;
                self.label_count += 1;
                self.gen_expr(condition);
                self.output.push(format!("  cmp $0, %rax"));
                self.output.push(format!("  je .L.else.{}", c));
                self.gen_stmt(&then_clause);
                self.output.push(format!("  jmp .L.end.{}", c));
                self.output.push(format!(".L.else.{}:", c));
                if let Some(els) = else_clause {
                    self.gen_stmt(els);
                }
                self.output.push(format!(".L.end.{}:", c));
            }
            ast::Stmt::ForStmt {
                init,
                condition,
                body,
                increment,
                break_label,
                continue_label,
            } => {
                let c = self.label_count;
                self.label_count += 1;
                self.gen_stmt(init);
                self.output.push(format!(".L.begin.{}:", c));
                if let Some(cond) = condition {
                    self.gen_expr(cond);
                    self.output.push(format!("  cmp $0, %rax"));
                    if let Some(brk_label) = break_label {
                        self.output.push(format!("  je {}", brk_label));
                    }
                }
                self.gen_stmt(body);
                if let Some(cont_label) = continue_label {
                    self.output.push(format!("{}:", cont_label));
                }
                if let Some(inc) = increment {
                    self.gen_expr(inc);
                }
                self.output.push(format!("  jmp .L.begin.{}", c));
                if let Some(brk_label) = break_label {
                    self.output.push(format!("{}:", brk_label));
                }
            }
            ast::Stmt::WhileStmt {
                condition,
                body,
                break_label,
                continue_label,
            } => {
                let c = self.label_count;
                self.label_count += 1;
                self.output.push(format!(".L.begin.{}:", c));
                self.gen_expr(condition);
                self.output.push(format!("  cmp $0, %rax"));
                if let Some(brk_label) = break_label {
                    self.output.push(format!("  je {}", brk_label));
                }
                self.gen_stmt(body);
                if let Some(cont_label) = continue_label {
                    self.output.push(format!("{}:", cont_label));
                }
                self.output.push(format!(" jmp .L.begin.{}", c));
                if let Some(brk_label) = break_label {
                    self.output.push(format!("{}:", brk_label));
                }
            }
            ast::Stmt::DoWhileStmt {
                condition,
                body,
                break_label,
                continue_label,
            } => {
                let c = self.label_count;
                self.label_count += 1;
                self.output.push(format!(".L.begin.{}:", c));
                self.gen_stmt(body);
                if let Some(cont_label) = continue_label {
                    self.output.push(format!("{}:", cont_label));
                }
                self.gen_expr(condition);
                self.output.push(format!("  cmp $0, %rax"));
                self.output.push(format!(" jne .L.begin.{}", c));
                if let Some(brk_label) = break_label {
                    self.output.push(format!("{}:", brk_label));
                }
            }
            ast::Stmt::GotoStmt { label } => {
                self.output
                    .push(format!("  jmp {}", self.goto_labels.get(label).unwrap()));
            }
            ast::Stmt::LabelStmt { label, stmt } => {
                self.output
                    .push(format!("{}:", self.goto_labels.get(label).unwrap()));
                self.gen_stmt(stmt);
            }
            ast::Stmt::SwitchStmt {
                condition,
                cases,
                default_case,
                break_label,
                body,
            } => {
                self.gen_expr(condition);

                for n in cases {
                    let reg = if condition.node.ty.get_size() == 8 {
                        "%rax"
                    } else {
                        "%eax"
                    };
                    self.output.push(format!("  cmp ${}, {}", n.get_val(), reg));
                    self.output.push(format!("  je {}", n.get_label()));
                }

                if let Some(default_case) = default_case {
                    self.output
                        .push(format!("  jmp {}", default_case.get_label()));
                }

                self.output
                    .push(format!("  jmp {}", break_label.clone().unwrap()));
                self.gen_stmt(&body.clone().unwrap());
                self.output
                    .push(format!("{}:", break_label.clone().unwrap()));
            }
            ast::Stmt::CaseStmt {
                label,
                val: _,
                stmt,
            } => {
                self.output.push(format!("{}:", label));
                self.gen_stmt(&stmt);
            }
        }
    }

    fn gen_expr(&mut self, ast: &ast::ExprWithPos) {
        self.output.push(format!("  .loc 1 {}", ast.pos.line));
        match &ast.node.node {
            ast::Expr::Null => (),
            ast::Expr::ConstInt { value, .. } => {
                self.output.push(format!("  mov ${}, %rax", value))
            }
            ast::Expr::ConstUInt { value, .. } => {
                self.output.push(format!("  mov ${}, %rax", value))
            }
            ast::Expr::ConstLong { value, .. } => {
                self.output.push(format!("  mov ${}, %rax", value))
            }
            ast::Expr::ConstULong { value, .. } => {
                self.output.push(format!("  mov ${}, %rax", value))
            }
            ast::Expr::ConstFloat { value } => {
                self.output
                    .push(format!("  mov ${}, %rax # float {}", *value as u32, value));
                self.output.push(format!("  movq %rax, %xmm0"));
            }
            ast::Expr::ConstDouble { value } => {
                self.output
                    .push(format!("  mov ${}, %rax # double {}", *value as u64, value));
                self.output.push(format!("  movq %rax, %xmm0"));
            }
            ast::Expr::Unary { expr, op } => match op.node {
                ast::UnaryOperator::Neg => {
                    self.gen_expr(expr);
                    self.output.push(format!("  neg %rax"));
                }
                ast::UnaryOperator::Not => {
                    self.gen_expr(expr);
                    self.output.push(format!("  cmp $0, %rax"));
                    self.output.push(format!("  sete %al"));
                    self.output.push(format!("  movzx %al, %rax"));
                }
                ast::UnaryOperator::BitNot => {
                    self.gen_expr(expr);
                    self.output.push(format!("  not %rax"));
                }
            },
            ast::Expr::Variable { .. } | ast::Expr::MemberExpr { .. } => {
                self.gen_addr(ast);
                self.load(&ast.node.ty);
            }
            ast::Expr::CastExpr { expr, ty } => {
                self.gen_expr(&expr);
                self.cast(expr.node.ty.clone(), ty.clone());
            }
            ast::Expr::Assign {
                l_value, r_value, ..
            } => {
                self.gen_addr(l_value);
                self.push();
                self.gen_expr(r_value);
                self.store(&ast.node.ty);
            }
            ast::Expr::Deref { expr, .. } => {
                self.gen_expr(expr);
                self.load(&ast.node.ty);
            }
            ast::Expr::Addr { expr, .. } => {
                self.gen_addr(expr);
            }
            ast::Expr::StmtExpr { body } => {
                for n in body {
                    self.gen_stmt(n);
                }
            }
            ast::Expr::CommaExpr { left, right } => {
                self.gen_expr(&left);
                self.gen_expr(&right);
            }
            ast::Expr::TernaryExpr {
                condition,
                then_clause,
                else_clause,
            } => {
                let c = self.label_count;
                self.label_count += 1;
                self.gen_expr(&condition);
                self.output.push(format!("  cmp $0, %rax"));
                self.output.push(format!("  je .L.else.{}", c));
                self.gen_expr(&then_clause);
                self.output.push(format!("  jmp .L.end.{}", c));
                self.output.push(format!(".L.else.{}:", c));
                self.gen_expr(&else_clause);
                self.output.push(format!(".L.end.{}:", c));
            }
            ast::Expr::FunctionCall { name, args } => {
                if args.len() > 0 {
                    // 参数逆序入栈
                    for arg in args.iter().rev() {
                        self.gen_expr(arg);
                        self.push();
                    }
                    for i in 0..=args.len() - 1 {
                        self.pop(ARGREG_64[i].to_string());
                    }
                }

                self.output.push(format!("  mov $0, %rax"));
                if self.depth % 2 == 0 {
                    self.output.push(format!("  call {}", name));
                } else {
                    self.output.push(format!("  sub $8, %rsp"));
                    self.output.push(format!("  call {}", name));
                    self.output.push(format!("  add $8, %rsp"));
                }

                match ast.node.ty.ty {
                    Ty::TyBool => self.output.push(format!("  movzx %al, %eax")),
                    Ty::TyChar => self.output.push(format!("  movsbl %al, %eax")),
                    Ty::TyUChar => self.output.push(format!("  movzbl %al, %eax")),
                    Ty::TyShort => self.output.push(format!("  movswl %ax, %eax")),
                    Ty::TyUShort => self.output.push(format!("  movzwl %ax, %eax")),
                    _ => (),
                }
            }
            ast::Expr::MemZero { var } => {
                // `rep stosb` is equivalent to `memset(%rdi, %al, %rcx)`.
                self.output
                    .push(format!("  mov ${}, %rcx", var.borrow().ty.get_size()));
                self.output
                    .push(format!("  lea {}(%rbp), %rdi", var.borrow().offset));
                self.output.push(format!("  mov $0, %al"));
                self.output.push(format!("  rep stosb"));
            }
            ast::Expr::Binary {
                left, op, right, ..
            } => {
                // 后序遍历
                // 先遍历右子树，再遍历左子树，最后遍历根节点
                self.gen_expr(right);
                self.push();
                self.gen_expr(left);
                self.pop("%rdi".to_string());
                let (ax, di, dx) = match (left.node.ty.clone().ty, right.node.ty.clone().ty) {
                    (Ty::TyLong | Ty::TyULong | Ty::TyArray { .. } | Ty::TyPtr { .. }, _) => {
                        ("%rax", "%rdi", "%rdx")
                    }
                    (_, Ty::TyLong | Ty::TyULong | Ty::TyArray { .. } | Ty::TyPtr { .. }) => {
                        ("%rax", "%rdi", "%rdx")
                    }
                    _ => ("%eax", "%edi", "%edx"),
                };
                match op.node {
                    ast::BinaryOperator::Add => self.output.push(format!("  add {}, {}", di, ax)),
                    ast::BinaryOperator::Sub => self.output.push(format!("  sub {}, {}", di, ax)),
                    ast::BinaryOperator::Mul => self.output.push(format!("  imul {}, {}", di, ax)),
                    ast::BinaryOperator::Div => {
                        if ast.node.ty.is_unsigned() {
                            self.output.push(format!("  mov $0, {}", dx));
                            self.output.push(format!("  div {}", di));
                        } else {
                            if left.node.ty.get_size() == 8 {
                                self.output.push(format!("  cqo"));
                            } else {
                                self.output.push(format!("  cdq"));
                            }
                            self.output.push(format!("  idiv {}", di));
                        }
                    }
                    ast::BinaryOperator::Mod => {
                        if ast.node.ty.is_unsigned() {
                            self.output.push(format!("  mov $0, {}", dx));
                            self.output.push(format!("  div {}", di));
                        } else {
                            if left.node.ty.get_size() == 8 {
                                self.output.push(format!("  cqo"));
                            } else {
                                self.output.push(format!("  cdq"));
                            }
                            self.output.push(format!("  idiv {}", di));
                        }

                        self.output.push(format!("  mov %rdx, %rax"));
                    }
                    ast::BinaryOperator::Eq => {
                        self.output.push(format!("  cmp {}, {}", di, ax));
                        self.output.push(format!("  sete %al"));
                        self.output.push(format!("  movzb %al, %rax"));
                    }
                    ast::BinaryOperator::Ne => {
                        self.output.push(format!("  cmp {}, {}", di, ax));
                        self.output.push(format!("  setne %al"));
                        self.output.push(format!("  movzb %al, %rax"));
                    }
                    ast::BinaryOperator::Lt => {
                        self.output.push(format!("  cmp {}, {}", di, ax));
                        if left.node.ty.is_unsigned() {
                            self.output.push(format!("  setb %al"));
                        } else {
                            self.output.push(format!("  setl %al"));
                        }
                        self.output.push(format!("  movzb %al, %rax"));
                    }
                    ast::BinaryOperator::Le => {
                        self.output.push(format!("  cmp {}, {}", di, ax));
                        if left.node.ty.is_unsigned() {
                            self.output.push(format!("  setbe %al"));
                        } else {
                            self.output.push(format!("  setle %al"));
                        }
                        self.output.push(format!("  movzb %al, %rax"));
                    }
                    ast::BinaryOperator::Gt => {
                        self.output.push(format!("  cmp {}, {}", di, ax));
                        if left.node.ty.is_unsigned() {
                            self.output.push(format!("  seta %al"));
                        } else {
                            self.output.push(format!("  setg %al"));
                        }
                        self.output.push(format!("  movzb %al, %rax"));
                    }
                    ast::BinaryOperator::Ge => {
                        self.output.push(format!("  cmp {}, {}", di, ax));
                        if left.node.ty.is_unsigned() {
                            self.output.push(format!("  setbe %al"));
                        } else {
                            self.output.push(format!("  setge %al"));
                        }
                        self.output.push(format!("  movzb %al, %rax"));
                    }
                    ast::BinaryOperator::BitAnd => self.output.push(format!("  and %rdi, %rax")),
                    ast::BinaryOperator::BitOr => self.output.push(format!("  or %rdi, %rax")),
                    ast::BinaryOperator::BitXor => self.output.push(format!("  xor %rdi, %rax")),
                    ast::BinaryOperator::LogAnd => {
                        let c = self.label_count;
                        self.label_count += 1;
                        self.gen_expr(left);
                        self.output.push(format!("  cmp $0, %rax"));
                        self.output.push(format!("  je .L.false.{}", c));
                        self.output.push(format!("  mov $1, %rax"));
                        self.output.push(format!("  jmp .L.end.{}", c));
                        self.output.push(format!(".L.false.{}:", c));
                        self.output.push(format!("  mov $0, %rax"));
                        self.output.push(format!(".L.end.{}:", c));
                    }
                    ast::BinaryOperator::LogOr => {
                        let c = self.label_count;
                        self.label_count += 1;
                        self.gen_expr(left);
                        self.output.push(format!("  cmp $0, %rax"));
                        self.output.push(format!("  jne .L.true.{}", c));
                        self.gen_expr(right);
                        self.output.push(format!("  cmp $0, %rax"));
                        self.output.push(format!("  jne .L.true.{}", c));
                        self.output.push(format!("  mov $0, %rax"));
                        self.output.push(format!("  jmp .L.end.{}", c));
                        self.output.push(format!(".L.true.{}:", c));
                        self.output.push(format!("  mov $1, %rax"));
                        self.output.push(format!(".L.end.{}:", c));
                    }
                    ast::BinaryOperator::SHL => {
                        self.output.push(format!("  mov %rdi, %rcx"));
                        self.output.push(format!("  shl %cl, {}", ax));
                    }
                    ast::BinaryOperator::SHR => {
                        self.output.push(format!("  mov %rdi, %rcx"));
                        if left.node.ty.is_unsigned() {
                            self.output.push(format!("  shr %cl, {}", ax));
                        } else {
                            self.output.push(format!("  sar %cl, {}", ax));
                        }
                    }
                }
            }
        }
    }

    fn assign_lvar_offsets(&mut self, ast: &mut ast::Program) {
        for f in &mut ast.funcs {
            let mut offset = 0;
            for local in &mut f.locals {
                offset += local.borrow().ty.get_size();
                offset = sema::align_to(offset, local.borrow().align);
                local.borrow_mut().offset = -offset;
            }
            f.stack_size = sema::align_to(offset, 16);
        }
    }

    fn emit_data(&mut self, ast: &mut ast::Program) {
        for global in &mut ast.globals {
            if !global.borrow().is_definition {
                continue;
            }
            if global.borrow().is_static {
                self.output.push(format!(
                    "  .local {}",
                    global.borrow().name.clone().unwrap()
                ));
            } else {
                self.output.push(format!(
                    "  .globl {}",
                    global.borrow().name.clone().unwrap()
                ));
            }

            self.output
                .push(format!("  .align {}", global.borrow().align));
            match &global.borrow().init_data {
                Some(val) => {
                    self.output.push(format!("  .data"));
                    self.output
                        .push(format!("{}:", global.borrow().name.clone().unwrap()));
                    let rels = &global.borrow().rel;
                    let mut pos = 0;
                    let mut i = 0;
                    while pos < global.borrow().ty.get_size() {
                        let r = rels.get(i);
                        if r.is_some() && r.unwrap().offset == pos {
                            self.output.push(format!(
                                "  .quad {}{:+}",
                                r.unwrap().label,
                                r.unwrap().addend
                            ));
                            i += 1;
                            pos += 8;
                        } else {
                            match val {
                                InitData::StringInitData(s) => self.output.push(format!(
                                    "  .byte {}",
                                    s.as_bytes().get(pos as usize).unwrap()
                                )),
                                InitData::BytesInitData(bytes) => self
                                    .output
                                    .push(format!("  .byte {}", bytes.get(pos as usize).unwrap())),
                                InitData::IntInitData(i) => {
                                    let mut bytes: [u8; 4] = [0, 0, 0, 0];
                                    bytes[0] = *i as u8;
                                    bytes[1] = (*i >> 8) as u8;
                                    bytes[2] = (*i >> 16) as u8;
                                    bytes[3] = (*i >> 24) as u8;
                                    self.output.push(format!(
                                        "  .byte {}",
                                        bytes.get(pos as usize).unwrap()
                                    ));
                                }
                            }
                            pos += 1;
                        }
                    }
                    continue;
                }
                _ => {
                    self.output.push(format!("  .bss"));
                    self.output
                        .push(format!("{}:", global.borrow().name.clone().unwrap()));
                    self.output
                        .push(format!("  .zero {}", global.borrow().ty.get_size()));
                }
            }
        }
    }

    fn emit_text(&mut self, ast: &mut ast::Program) {
        for f in &mut ast.funcs {
            self.goto_labels = f.goto_labels.clone();
            if !f.is_definition {
                continue;
            }
            if f.is_static {
                self.output
                    .push(format!("  .local {}", f.name.clone().unwrap()));
            } else {
                self.output
                    .push(format!("  .globl {}", f.name.clone().unwrap()));
            }
            self.output.push(format!("  .text"));
            self.output.push(format!("{}:", f.name.clone().unwrap()));
            self.current_fn = Some(f.clone());

            // Prologue
            self.output.push(format!("  push %rbp"));
            self.output.push(format!("  mov %rsp, %rbp"));
            self.output.push(format!("  sub ${}, %rsp", f.stack_size));

            // Save arg registers if function is variadic
            if let Some(va_area) = &f.va_area {
                let gp = f.params.len();
                let off = va_area.borrow().offset;

                // va_elem
                self.output
                    .push(format!("  movl ${}, {}(%rbp)", gp * 8, off));
                self.output.push(format!("  movl $0, {}(%rbp)", off + 4));
                self.output.push(format!("  movq %rbp, {}(%rbp)", off + 16));
                self.output
                    .push(format!("  addq ${}, {}(%rbp)", off + 24, off + 16));

                // __reg_save_area__
                self.output.push(format!("  movq %rdi, {}(%rbp)", off + 24));
                self.output.push(format!("  movq %rsi, {}(%rbp)", off + 32));
                self.output.push(format!("  movq %rdx, {}(%rbp)", off + 40));
                self.output.push(format!("  movq %rcx, {}(%rbp)", off + 48));
                self.output.push(format!("  movq %r8, {}(%rbp)", off + 56));
                self.output.push(format!("  movq %r9, {}(%rbp)", off + 64));
                self.output
                    .push(format!("  movsd %xmm0, {}(%rbp)", off + 72));
                self.output
                    .push(format!("  movsd %xmm1, {}(%rbp)", off + 80));
                self.output
                    .push(format!("  movsd %xmm2, {}(%rbp)", off + 88));
                self.output
                    .push(format!("  movsd %xmm3, {}(%rbp)", off + 96));
                self.output
                    .push(format!("  movsd %xmm4, {}(%rbp)", off + 104));
                self.output
                    .push(format!("  movsd %xmm5, {}(%rbp)", off + 112));
                self.output
                    .push(format!("  movsd %xmm6, {}(%rbp)", off + 120));
                self.output
                    .push(format!("  movsd %xmm7, {}(%rbp)", off + 128));
            }

            // Save passed-by-register arguments to the stack
            let mut i = 0;
            for p in &mut f.params.iter().rev() {
                let offset = p.borrow().offset;
                self.store_gp(i, offset, p.borrow().ty.get_size());
                i += 1;
            }

            // Emit code
            self.gen_stmt(&f.body);
            assert!(self.depth == 0);

            // Epilogue
            self.output
                .push(format!(".L.return.{}:", f.name.clone().unwrap()));
            self.output.push(format!("  mov %rbp, %rsp"));
            self.output.push(format!("  pop %rbp"));
            self.output.push(format!("  ret"));
        }
    }

    pub fn codegen(&mut self, ast: &mut ast::Program) {
        let r = writeln!(self.out_writer, ".file 1 \"{}\"", self.file_path);
        match r {
            Ok(_) => (),
            _ => panic!("codegen error."),
        }
        self.assign_lvar_offsets(ast);
        self.emit_data(ast);
        self.emit_text(ast);

        for i in &self.output {
            let r = writeln!(self.out_writer, "{}", i);
            match r {
                Ok(_) => (),
                _ => panic!("codegen error."),
            }
        }
    }
}
