use std::{env, io::BufReader, rc::Rc};

use lexer::Lexer;
use symbol::{Strings, Symbols};
use token::{Tok, Token};

mod error;
mod lexer;
mod position;
mod symbol;
mod terminal;
mod token;

fn main() {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        panic!("{}: invalid number of arguments\n", args.first().unwrap());
    }

    let strings = Rc::new(Strings::new());
    let mut symbols: Symbols<()> = Symbols::new(Rc::clone(&strings));
    let mut source_code = args.get(1).unwrap().clone();
    if source_code.as_bytes().last().unwrap() == &b'\n' {
        source_code.push_str("\n");
    } else {
        source_code.push_str("\n\0");
    }
    let file = BufReader::new(source_code.as_bytes());
    let file_symbol = symbols.symbol("stdin");
    let mut lexer = Lexer::new(file, file_symbol);

    println!("  .globl main");
    println!("main:");

    // the first token must be a number
    println!("  mov ${}, %rax", lexer.token().unwrap().token);

    // ... followed by either `+ <number>` or `- <number>`.
    loop {
        match lexer.token() {
            Ok(Token {
                token: Tok::Plus, ..
            }) => println!("  add ${}, %rax", lexer.token().unwrap().token),
            Ok(Token {
                token: Tok::Minus, ..
            }) => println!("  sub ${}, %rax", lexer.token().unwrap().token),
            _ => break,
        }
    }

    println!("  ret");
}
