use crate::ast;

pub struct CodeGenerator {
    pub depth: u32,
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

    pub fn gen_expr(&mut self, ast: ast::ExprWithPos) {
        match ast.node {
            ast::Expr::Number { value } => println!("  mov ${}, %rax", value),
            ast::Expr::Binary { left, op, right } => {
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
                }
            }
        }
    }
}
