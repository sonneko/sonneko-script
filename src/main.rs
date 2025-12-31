mod lexer;
mod parser;
mod engine;

use std::{env, fs};

fn main() {
    let args: Vec<String> = env::args().collect();
    assert!(args.len() == 2, "Too many arguments provided.");
    let file_path = &*args[1];
    let content = fs::read_to_string(file_path).expect("can't open the file");
    execute(content);
}

fn execute(source: String) {
    let mut lexer = lexer::Lexer::new(&source);
    let tokens = lexer.tokenize();
    println!("{:?}", tokens);
    let mut parser = parser::Parser::new(tokens);
    let ast = parser.parse_program();
    let engine = engine::Interpreter::new(ast);
    engine.run();
}