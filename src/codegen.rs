use std::io::Write;

use crate::{
    ast::{self, Function, InitData},
    sema::{self, get_sizeof, Type},
};

static ARGREG_64: [&str; 6] = ["%rdi", "%rsi", "%rdx", "%rcx", "%r8", "%r9"];
static ARGREG_32: [&str; 6] = ["%edi", "%esi", "%edx", "%ecx", "%r8d", "%r9d"];
static ARGREG_16: [&str; 6] = ["%di", "%si", "%dx", "%cx", "%r8w", "%r9w"];
static ARGREG_8: [&str; 6] = ["%dil", "%sil", "%dl", "%cl", "%r8b", "%r9b"];

pub struct CodeGenerator {
    depth: u32,
    label_count: u32,
    current_fn: Option<Function>,
    output: Vec<String>,
    out_writer: Box<dyn Write>,

    file_path: String,
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
        match ty {
            // If it is an array, do not attempt to load a value to the
            // register because in general we can't load an entire array to a
            // register. As a result, the result of an evaluation of an array
            // becomes not the array itself but the address of the array.
            // This is where "array is automatically converted to a pointer to
            // the first element of the array in C" occurs.
            Type::TyArray { .. } => (),
            Type::TyStruct { .. } => (),
            Type::TyUnion { .. } => (),
            _ => match get_sizeof(ty.clone()) {
                1 => self.output.push(format!("  movsbq (%rax), %rax")),
                2 => self.output.push(format!("  movswq (%rax), %rax")),
                4 => self.output.push(format!("  movsxd (%rax), %rax")),
                _ => self.output.push(format!("  mov (%rax), %rax")),
            },
        }
    }

    // Store %rax to an address that the stack top is pointing to.
    fn store(&mut self, ty: &Type) {
        self.pop("%rdi".to_string());
        match ty {
            Type::TyStruct { type_size, .. } | Type::TyUnion { type_size, .. } => {
                for i in 0..=type_size - 1 {
                    self.output.push(format!("  mov {}(%rax), %r8b", i));
                    self.output.push(format!("  mov %r8b, {}(%rdi)", i));
                }
            }
            _ => match get_sizeof(ty.clone()) {
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

    fn gen_addr(&mut self, ast: &ast::ExprWithPos) {
        match &ast.node.node {
            ast::Expr::Variable { obj, .. } => {
                if obj.borrow().is_local {
                    self.output
                        .push(format!("  lea {}(%rbp), %rax", obj.borrow().offset));
                } else {
                    self.output
                        .push(format!("  lea {}(%rip), %rax", obj.borrow().name));
                }
            }
            ast::Expr::Deref { expr, .. } => self.gen_expr(expr),
            ast::Expr::CommaExpr { left, right } => {
                self.gen_expr(&left);
                self.gen_addr(&right);
            }
            ast::Expr::MemberExpr { strct, member } => {
                self.gen_addr(&strct);
                self.output
                    .push(format!("  add ${}, %rax", member.borrow().offset));
            }
            _ => panic!("not an lvalue"),
        }
    }

    fn gen_stmt(&mut self, ast: &ast::StmtWithPos) {
        self.output.push(format!("  .loc 1 {}", ast.pos.line));
        match &ast.node {
            ast::Stmt::ExprStmt { expr } => self.gen_expr(expr),
            ast::Stmt::Return { expr } => {
                self.gen_expr(expr);
                self.output.push(format!(
                    "  jmp .L.return.{}",
                    self.current_fn.as_ref().unwrap().name
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
            } => {
                let c = self.label_count;
                self.label_count += 1;
                self.gen_stmt(init);
                self.output.push(format!(".L.begin.{}:", c));
                if let Some(cond) = condition {
                    self.gen_expr(cond);
                    self.output.push(format!("  cmp $0, %rax"));
                    self.output.push(format!("  je .L.end.{}", c));
                }
                self.gen_stmt(body);
                if let Some(inc) = increment {
                    self.gen_expr(inc);
                }
                self.output.push(format!("  jmp .L.begin.{}", c));
                self.output.push(format!(".L.end.{}:", c));
            }
            ast::Stmt::WhileStmt { condition, body } => {
                let c = self.label_count;
                self.label_count += 1;
                self.output.push(format!(".L.begin.{}:", c));
                self.gen_expr(condition);
                self.output.push(format!("  cmp $0, %rax"));
                self.output.push(format!("  je .L.end.{}", c));
                self.gen_stmt(body);
                self.output.push(format!(" jmp .L.begin.{}", c));
                self.output.push(format!(".L.end.{}:", c));
            }
        }
    }

    fn gen_expr(&mut self, ast: &ast::ExprWithPos) {
        self.output.push(format!("  .loc 1 {}", ast.pos.line));
        match &ast.node.node {
            ast::Expr::Number { value, .. } => self.output.push(format!("  mov ${}, %rax", value)),
            ast::Expr::Unary { expr, .. } => {
                self.gen_expr(expr);
                self.output.push(format!("  neg %rax"));
            }
            ast::Expr::Variable { .. } | ast::Expr::MemberExpr { .. } => {
                self.gen_addr(ast);
                self.load(&ast.node.ty);
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
                self.output.push(format!("  call {}", name));
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
                let (ax, di) = match (left.node.ty.clone(), right.node.ty.clone()) {
                    (Type::TyLong { .. } | Type::TyArray { .. } | Type::TyPtr { .. }, _) => {
                        ("%rax", "%rdi")
                    }
                    (_, Type::TyLong { .. } | Type::TyArray { .. } | Type::TyPtr { .. }) => {
                        ("%rax", "%rdi")
                    }
                    _ => ("%eax", "%edi"),
                };
                match op.node {
                    ast::BinaryOperator::Add => self.output.push(format!("  add {}, {}", di, ax)),
                    ast::BinaryOperator::Sub => self.output.push(format!("  sub {}, {}", di, ax)),
                    ast::BinaryOperator::Mul => self.output.push(format!("  imul {}, {}", di, ax)),
                    ast::BinaryOperator::Div => {
                        if get_sizeof(left.node.ty.clone()) == 8 {
                            self.output.push(format!("  cqo"));
                        } else {
                            self.output.push(format!("  cdq"));
                        }
                        self.output.push(format!("  idiv {}", di));
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
                        self.output.push(format!("  setl %al"));
                        self.output.push(format!("  movzb %al, %rax"));
                    }
                    ast::BinaryOperator::Le => {
                        self.output.push(format!("  cmp {}, {}", di, ax));
                        self.output.push(format!("  setle %al"));
                        self.output.push(format!("  movzb %al, %rax"));
                    }
                    ast::BinaryOperator::Gt => {
                        self.output.push(format!("  cmp {}, {}", di, ax));
                        self.output.push(format!("  setg %al"));
                        self.output.push(format!("  movzb %al, %rax"));
                    }
                    ast::BinaryOperator::Ge => {
                        self.output.push(format!("  cmp {}, {}", di, ax));
                        self.output.push(format!("  setge %al"));
                        self.output.push(format!("  movzb %al, %rax"));
                    }
                }
            }
        }
    }

    fn assign_lvar_offsets(&mut self, ast: &mut ast::Program) {
        for f in &mut ast.funcs {
            let mut offset = 0;
            for local in &mut f.locals {
                offset += get_sizeof(local.borrow().ty.clone());
                offset = sema::align_to(offset, sema::get_align(local.borrow().ty.clone()));
                local.borrow_mut().offset = -offset;
            }
            f.stack_size = sema::align_to(offset, 16);
        }
    }

    fn emit_data(&mut self, ast: &mut ast::Program) {
        for global in &mut ast.globals {
            self.output.push(format!("  .data"));
            self.output
                .push(format!("  .globl {}", global.borrow().name));
            self.output.push(format!("{}:", global.borrow().name));
            match &global.borrow().init_data {
                Some(InitData::StringInitData(s)) => {
                    for c in s.chars() {
                        self.output.push(format!("  .byte {}", c as u8));
                    }
                }
                _ => self.output.push(format!(
                    "  .zero {}",
                    get_sizeof(global.borrow().ty.clone())
                )),
            }
        }
    }

    fn emit_text(&mut self, ast: &mut ast::Program) {
        for f in &mut ast.funcs {
            self.output.push(format!("  .globl {}", f.name));
            self.output.push(format!("  .text"));
            self.output.push(format!("{}:", f.name));
            self.current_fn = Some(f.clone());

            // Prologue
            self.output.push(format!("  push %rbp"));
            self.output.push(format!("  mov %rsp, %rbp"));
            self.output.push(format!("  sub ${}, %rsp", f.stack_size));

            // Save passed-by-register arguments to the stack
            let mut i = 0;
            for p in &mut f.params.iter().rev() {
                let offset = p.borrow().offset;
                self.store_gp(i, offset, get_sizeof(p.borrow().ty.clone()));
                i += 1;
            }

            // Emit code
            self.gen_stmt(&f.body);
            assert!(self.depth == 0);

            // Epilogue
            self.output.push(format!(".L.return.{}:", f.name));
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
