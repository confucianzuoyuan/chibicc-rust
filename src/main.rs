use std::env;

fn main() {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        panic!("{}: invalid number of arguments\n", args.first().unwrap());
    }

    println!("  .globl main");
    println!("main:");

    let mut p = args.get(1).unwrap().as_bytes().into_iter().peekable();

    // 首先提取第一个整型
    let mut i = 0;
    while p.peek().is_some() {
        if p.peek().unwrap().is_ascii_digit() {
            i = i * 10 + (*p.next().unwrap() as u32 - '0' as u32);
        } else {
            break;
        }
    }

    println!("  mov ${}, %rax", i);

    // 判断运算符
    loop {
        if let Some(&ch) = p.peek() {
            match ch {
                b'+' => {
                    p.next();
                    let mut i = 0;
                    while p.peek().is_some() {
                        if p.peek().unwrap().is_ascii_digit() {
                            i = i * 10 + (*p.next().unwrap() as u32 - '0' as u32);
                        } else {
                            break;
                        }
                    }
                    println!("  add ${}, %rax", i);
                }
                b'-' => {
                    p.next();
                    let mut i = 0;
                    while p.peek().is_some() {
                        if p.peek().unwrap().is_ascii_digit() {
                            i = i * 10 + (*p.next().unwrap() as u32 - '0' as u32);
                        } else {
                            break;
                        }
                    }
                    println!("  sub ${}, %rax", i);
                }
                _ => panic!("unexpected character: '{}'", *ch),
            }
        } else {
            break;
        }
    }

    println!("  ret");
}
