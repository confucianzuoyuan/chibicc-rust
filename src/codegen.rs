use crate::{
    ast::{self, Function, InitData},
    sema::{get_sizeof, Type},
    token::{Tok, Token},
};

static ARGREG_64: [&str; 6] = ["%rdi", "%rsi", "%rdx", "%rcx", "%r8", "%r9"];
static ARGREG_8: [&str; 6] = ["%dil", "%sil", "%dl", "%cl", "%r8b", "%r9b"];

pub struct CodeGenerator {
    depth: u32,
    label_count: u32,
    current_fn: Option<Function>,
}

impl CodeGenerator {
    pub fn new() -> Self {
        CodeGenerator {
            depth: 0,
            label_count: 1,
            current_fn: None,
        }
    }

    fn push(&mut self) {
        println!("  push %rax");
        self.depth += 1;
    }

    fn pop(&mut self, arg: String) {
        println!("  pop {}", arg);
        self.depth -= 1;
    }

    // Load a value from where %rax is pointing to.
    fn load(&self, ty: &Type) {
        match ty {
            // If it is an array, do not attempt to load a value to the
            // register because in general we can't load an entire array to a
            // register. As a result, the result of an evaluation of an array
            // becomes not the array itself but the address of the array.
            // This is where "array is automatically converted to a pointer to
            // the first element of the array in C" occurs.
            Type::TyArray { .. } => (),
            _ => {
                if get_sizeof(ty.clone()) == 1 {
                    println!("  movsbq (%rax), %rax");
                } else {
                    println!("  mov (%rax), %rax");
                }
            }
        }
    }

    // Store %rax to an address that the stack top is pointing to.
    fn store(&mut self, ty: &Type) {
        self.pop("%rdi".to_string());
        if get_sizeof(ty.clone()) == 1 {
            println!("  mov %al, (%rdi)");
        } else {
            println!("  mov %rax, (%rdi)");
        }
    }

    fn gen_addr(&mut self, ast: &ast::ExprWithPos) {
        match &ast.node.node {
            ast::Expr::Variable { obj, .. } => {
                if obj.borrow().is_local {
                    println!("  lea {}(%rbp), %rax", obj.borrow().offset);
                } else {
                    println!("  lea {}(%rip), %rax", obj.borrow().name);
                }
            }
            ast::Expr::Deref { expr, .. } => self.gen_expr(expr),
            _ => panic!("not an lvalue"),
        }
    }

    fn gen_stmt(&mut self, ast: &ast::StmtWithPos) {
        match &ast.node {
            ast::Stmt::ExprStmt { expr } => self.gen_expr(expr),
            ast::Stmt::Return { expr } => {
                self.gen_expr(expr);
                println!("  jmp .L.return.{}", self.current_fn.as_ref().unwrap().name);
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
                println!("  cmp $0, %rax");
                println!("  je .L.else.{}", c);
                self.gen_stmt(&then_clause);
                println!("  jmp .L.end.{}", c);
                println!(".L.else.{}:", c);
                if let Some(els) = else_clause {
                    self.gen_stmt(els);
                }
                println!(".L.end.{}:", c);
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
                println!(".L.begin.{}:", c);
                if let Some(cond) = condition {
                    self.gen_expr(cond);
                    println!("  cmp $0, %rax");
                    println!("  je .L.end.{}", c);
                }
                self.gen_stmt(body);
                if let Some(inc) = increment {
                    self.gen_expr(inc);
                }
                println!("  jmp .L.begin.{}", c);
                println!(".L.end.{}:", c);
            }
            ast::Stmt::WhileStmt { condition, body } => {
                let c = self.label_count;
                self.label_count += 1;
                println!(".L.begin.{}:", c);
                self.gen_expr(condition);
                println!("  cmp $0, %rax");
                println!("  je .L.end.{}", c);
                self.gen_stmt(body);
                println!(" jmp .L.begin.{}", c);
                println!(".L.end.{}:", c);
            }
        }
    }

    fn gen_expr(&mut self, ast: &ast::ExprWithPos) {
        match &ast.node.node {
            ast::Expr::Number { value, .. } => println!("  mov ${}, %rax", value),
            ast::Expr::Unary { expr, .. } => {
                self.gen_expr(expr);
                println!("  neg %rax");
            }
            ast::Expr::Variable { .. } => {
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

                println!("  mov $0, %rax");
                println!("  call {}", name);
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
                match op.node {
                    ast::BinaryOperator::Add => println!("  add %rdi, %rax"),
                    ast::BinaryOperator::Sub => println!("  sub %rdi, %rax"),
                    ast::BinaryOperator::Mul => println!("  imul %rdi, %rax"),
                    ast::BinaryOperator::Div => {
                        println!("  cqo");
                        println!("  idiv %rdi");
                    }
                    ast::BinaryOperator::Eq => {
                        println!("  cmp %rdi, %rax");
                        println!("  sete %al");
                        println!("  movzb %al, %rax");
                    }
                    ast::BinaryOperator::Ne => {
                        println!("  cmp %rdi, %rax");
                        println!("  setne %al");
                        println!("  movzb %al, %rax");
                    }
                    ast::BinaryOperator::Lt => {
                        println!("  cmp %rdi, %rax");
                        println!("  setl %al");
                        println!("  movzb %al, %rax");
                    }
                    ast::BinaryOperator::Le => {
                        println!("  cmp %rdi, %rax");
                        println!("  setle %al");
                        println!("  movzb %al, %rax");
                    }
                    ast::BinaryOperator::Gt => {
                        println!("  cmp %rdi, %rax");
                        println!("  setg %al");
                        println!("  movzb %al, %rax");
                    }
                    ast::BinaryOperator::Ge => {
                        println!("  cmp %rdi, %rax");
                        println!("  setge %al");
                        println!("  movzb %al, %rax");
                    }
                }
            }
        }
    }

    // Round up `n` to the nearest multiple of `align`. For instance,
    // align_to(5, 8) returns 8 and align_to(11, 8) returns 16.
    fn align_to(&self, n: i32, align: i32) -> i32 {
        (n + align - 1) / align * align
    }

    fn assign_lvar_offsets(&mut self, ast: &mut ast::Program) {
        use itertools::Itertools;
        for f in &mut ast.funcs {
            let mut offset = 0;
            for local in &mut f.locals.iter().sorted_by_key(|x| x.0).rev() {
                offset += get_sizeof(local.1.borrow().ty.clone());
                local.1.borrow_mut().offset = -offset;
            }
            f.stack_size = self.align_to(offset, 16);
        }
    }

    fn emit_data(&mut self, ast: &mut ast::Program) {
        for global in &mut ast.globals {
            println!("  .data");
            println!("  .globl {}", global.1.borrow().name);
            println!("{}:", global.1.borrow().name);
            match &global.1.borrow().init_data {
                Some(InitData::StringInitData(s)) => {
                    for c in s.as_bytes() {
                        println!("  .byte {}", c);
                    }
                    println!("  .byte {}", '\0');
                }
                _ => println!("  .zero {}", get_sizeof(global.1.borrow().ty.clone())),
            }
        }
    }

    fn emit_text(&mut self, ast: &mut ast::Program) {
        for f in &mut ast.funcs {
            println!("  .globl {}", f.name);
            println!("  .text");
            println!("{}:", f.name);
            self.current_fn = Some(f.clone());

            // Prologue
            println!("  push %rbp");
            println!("  mov %rsp, %rbp");
            println!("  sub ${}, %rsp", f.stack_size);

            // Save passed-by-register arguments to the stack
            let mut i = 0;
            use itertools::Itertools;
            for p in &mut f.params.iter().sorted_by_key(|x| x.0) {
                match p.1.borrow().ty.clone() {
                    Type::TyInt { name }
                    | Type::TyChar { name }
                    | Type::TyPtr { name, .. }
                    | Type::TyArray { name, .. } => {
                        if let Some(Token {
                            token: Tok::Ident(ident),
                            ..
                        }) = name
                        {
                            let offset = f.locals.get(&ident).unwrap().borrow().offset;
                            if get_sizeof(f.locals.get(&ident).unwrap().borrow().ty.clone()) == 1 {
                                println!("  mov {}, {}(%rbp)", ARGREG_8[i], offset);
                            } else {
                                println!("  mov {}, {}(%rbp)", ARGREG_64[i], offset);
                            }
                            i += 1;
                        }
                    }
                    _ => panic!(),
                }
            }

            // Emit code
            self.gen_stmt(&f.body);
            assert!(self.depth == 0);

            // Epilogue
            println!(".L.return.{}:", f.name);
            println!("  mov %rbp, %rsp");
            println!("  pop %rbp");
            println!("  ret");
        }
    }

    pub fn codegen(&mut self, ast: &mut ast::Program) {
        self.assign_lvar_offsets(ast);
        self.emit_data(ast);
        self.emit_text(ast);
    }
}
