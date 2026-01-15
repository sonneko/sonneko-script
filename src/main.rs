mod ast;
mod engine;
mod parser;
mod lexer;
mod tokens;

use lexer::Lexer;
use parser::Parser;
use crate::engine::Interpreter;

// ==========================================
// 4. Main Entry Integration
// ==========================================

// 前回のParserでパースした後、この関数を呼び出す想定
pub fn run_interpreter_demo() {
    // デモ用のコード
    // Point::new の挙動を動かすため、EBNFでは定義できない実装を
    // インタープリタ側でモックアップとして注入する必要がありますが、
    // ここでは純粋にパース可能なコードで、かつ動くものに変更します。
    // EBNFにはメソッド定義構文がないため、関数ベースでオブジェクトを返します。
    
    let code = r#"
        fn make_point(x: Int, y: Int) {
            let p = 0; 
            return x + y;
        }

        fn main() {
            let val = 10;
            let res = make_point(val, 20);
            
            print("Calculation Result:", res);
            
            if res > 25 {
                print("Result is big!");
            } else {
                print("Result is small.");
            }

            let i = 0;
            while i < 3 {
                print("Counting:", i);
                let i = i + 1;
            }
        }
    "#;

    println!("--- Source Code ---");
    println!("{}", code);

    let lexer = Lexer::new(); // 前回のLexer
    let tokens = lexer.tokenize(code).unwrap();
    let mut parser = Parser::new(tokens); // 前回のParser
    
    match parser.parse_program() {
        Ok(program) => {
            println!("--- AST Parsed Successfully ---");
            let mut interpreter = Interpreter::new();
            println!("--- Execution Start ---");
            match interpreter.eval_program(&program) {
                Ok(_) => println!("--- Execution Finished ---"),
                Err(e) => eprintln!("Runtime Error: {}", e),
            }
        },
        Err(e) => eprintln!("Parse Error: {}", e),
    }
}

fn main() {
    run_interpreter_demo();
}