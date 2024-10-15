use std::{
    fs::{self, File}, io::{self, BufRead, BufReader, Write}, path::Path, rc::Rc
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

use clap::Parser as ClapParser;

#[derive(ClapParser)]
#[command(version, about, long_about = None)]
struct Cli {
    /// input c source code file
    filename: Option<String>,

    /// Sets a output asm file name
    #[arg(short, long)]
    output: Option<String>,
}

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
    let cli = Cli::parse();
    let out_writer = match cli.output.as_deref() {
        Some(x) => {
            let path = Path::new(x);
            Box::new(File::create(&path).unwrap()) as Box<dyn Write>
        }
        None => Box::new(io::stdout()) as Box<dyn Write>,
    };

    if let Some(filename) = cli.filename.as_deref() {
        let mut source_code = String::new();
        if filename == "-".to_string() {
            let stdin = io::stdin();
            let handle = stdin.lock();

            for line in handle.lines() {
                let line = line.expect("无法读取行。");
                source_code.push_str(line.as_str());
            }
        } else {
            source_code = fs::read_to_string(filename)?;
        }
        if source_code.as_bytes().last().unwrap() == &b'\n' {
            source_code.push_str("\n");
        } else {
            source_code.push_str("\n\0");
        }
        let file = BufReader::new(source_code.as_bytes());
        let file_symbol = symbols.symbol(filename);
        let lexer = Lexer::new(file, file_symbol);
        let mut parser = Parser::new(lexer, symbols);
        let mut ast = parser.parse()?;
        let mut cg = CodeGenerator::new(out_writer);

        cg.codegen(&mut ast);
    } else {
        println!("must have a c source code file.");
    }
    Ok(())
}
