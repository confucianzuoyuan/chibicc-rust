use crate::ast;

pub struct CodeGenerator {
    depth: u32,
}

impl CodeGenerator {
    pub fn new() -> Self {
        CodeGenerator { depth: 0 }
    }

    fn push(&mut self) {
        println!("  push %rax");
        self.depth += 1;
    }

    fn pop(&mut self, arg: String) {
        println!("  pop {}", arg);
        self.depth -= 1;
    }

    fn gen_addr(&mut self, ast: ast::ExprWithPos) {
        match ast.node {
            ast::Expr::Variable { name } => {
                let offset = name.chars().next().unwrap() as i32 - 'a' as i32 + 1;
                println!("  lea {}(%rbp), %rax", -offset);
            }
            _ => panic!("not an lvalue"),
        }
    }

    fn gen_stmt(&mut self, ast: ast::StmtWithPos) {
        match ast.node {
            ast::Stmt::ExprStmt { expr } => self.gen_expr(expr),
        }
    }

    fn gen_expr(&mut self, ast: ast::ExprWithPos) {
        match ast.node {
            ast::Expr::Number { value } => println!("  mov ${}, %rax", value),
            ast::Expr::Unary { expr, .. } => {
                self.gen_expr(*expr);
                println!("  neg %rax");
            }
            ast::Expr::Variable { .. } => {
                self.gen_addr(ast);
                println!("  mov (%rax), %rax");
            }
            ast::Expr::Assign { l_value, r_value } => {
                self.gen_addr(*l_value);
                self.push();
                self.gen_expr(*r_value);
                self.pop("%rdi".to_string());
                println!("  mov %rax, (%rdi)");
            }
            ast::Expr::Binary { left, op, right } => {
                // 后序遍历
                // 先遍历右子树，再遍历左子树，最后遍历根节点
                self.gen_expr(*right);
                self.push();
                self.gen_expr(*left);
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

    pub fn codegen(&mut self, ast: ast::Program) {
        println!("  .globl main");
        println!("main:");

        // Prologue
        println!("  push %rbp");
        println!("  mov %rsp, %rbp");
        println!("  sub $208, %rsp");

        for n in ast {
            self.gen_stmt(n);
            assert!(self.depth == 0);
        }

        println!("  mov %rbp, %rsp");
        println!("  pop %rbp");
        println!("  ret");
    }
}
