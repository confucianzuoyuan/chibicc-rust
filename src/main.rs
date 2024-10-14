use std::{
    fs, io::{self, BufRead, BufReader}, rc::Rc
};

use codegen::CodeGenerator;
use error::Error;
use lexer::Lexer;
use parser::Parser;
use symbol::{Strings, Symbols};
use terminal::Terminal;

mod ast;
mod codegen;
mod error;
mod lexer;
mod parser;
mod position;
mod sema;
mod symbol;
mod terminal;
mod token;

fn main() {
    let strings = Rc::new(Strings::new());
    let mut symbols = Symbols::new(Rc::clone(&strings));
    if let Err(error) = drive(strings, &mut symbols) {
        let terminal = Terminal::new();
        if let Err(error) = error.show(&symbols, &terminal) {
            eprintln!("Error printing errors: {}", error);
        }
    }
}

fn drive(strings: Rc<Strings>, symbols: &mut Symbols<()>) -> Result<(), Error> {
    let mut args = std::env::args();
    args.next();
    if let Some(filename) = args.next() {
        if filename == "-".to_string() {
            let stdin = io::stdin();
            let handle = stdin.lock();

            let mut source_code = String::new();
            for line in handle.lines() {
                let line = line.expect("无法读取行。");
                source_code.push_str(line.as_str());
            }

            if source_code.as_bytes().last().unwrap() == &b'\n' {
                source_code.push_str("\n");
            } else {
                source_code.push_str("\n\0");
            }
            let file = BufReader::new(source_code.as_bytes());
            let file_symbol = symbols.symbol("stdin");
            let lexer = Lexer::new(file, file_symbol);
            let mut parser = Parser::new(lexer, symbols);
            let mut ast = parser.parse()?;
            let mut cg = CodeGenerator::new();

            cg.codegen(&mut ast);
        } else {
            let mut source_code = fs::read_to_string(filename.clone())?;
            if source_code.as_bytes().last().unwrap() == &b'\n' {
                source_code.push_str("\n");
            } else {
                source_code.push_str("\n\0");
            }
            let file = BufReader::new(source_code.as_bytes());
            let file_symbol = symbols.symbol(filename.as_str());
            let lexer = Lexer::new(file, file_symbol);
            let mut parser = Parser::new(lexer, symbols);
            let mut ast = parser.parse()?;
            let mut cg = CodeGenerator::new();

            cg.codegen(&mut ast);
        }
    }
    Ok(())
}
