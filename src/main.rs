use std::{io::BufReader, rc::Rc};

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
    if let Some(source_code) = args.next() {
        let mut source_code = source_code.clone();
        if source_code.as_bytes().last().unwrap() == &b'\n' {
            source_code.push_str("\n");
        } else {
            source_code.push_str("\n\0");
        }
        let file = BufReader::new(source_code.as_bytes());
        let file_symbol = symbols.symbol("stdin");
        let lexer = Lexer::new(file, file_symbol);
        let mut parser = Parser::new(lexer, symbols);
        let ast = parser.parse()?;
        let mut cg = CodeGenerator::new();

        cg.codegen(ast);
    }
    Ok(())
}
