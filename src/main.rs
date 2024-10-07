use std::env;

fn main() {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        panic!("{}: invalid number of arguments\n", args.first().unwrap());
    }
    
    println!("  .globl main");
    println!("main:");
    println!("  mov ${}, %rax", args.get(1).unwrap());
    println!("  ret");
}
