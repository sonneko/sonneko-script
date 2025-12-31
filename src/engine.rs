use std::collections::HashMap;
use super::parser;

/// 変数や関数のスコープを管理する構造体
#[derive(Default, Clone)]
pub struct Environment {
    variables: HashMap<String, i64>,
}

pub struct Interpreter {
    functions: HashMap<String, parser::FunctionDecl>,
}

impl Interpreter {
    pub fn new(program: parser::Program) -> Self {
        let mut functions = HashMap::new();
        for f in program.functions {
            functions.insert(f.name.clone(), f);
        }
        Self { functions }
    }

    /// メイン関数から実行を開始する
    pub fn run(&self) -> i64 {
        let main_fn = self.functions.get("main").expect("main function not found");
        let mut env = Environment::default();
        self.execute_block(&main_fn.body, &mut env)
    }

    fn execute_block(&self, block: &parser::Block, env: &mut Environment) -> i64 {
        // 文を順番に実行
        for stmt in &block.statements {
            match stmt {
                parser::Statement::Let { name, value } => {
                    let val = self.eval_expr(value, env);
                    env.variables.insert(name.clone(), val);
                }
                parser::Statement::ExprStmt(expr) => {
                    self.eval_expr(expr, env);
                }
            }
        }

        // 最後の式があればその値を返し、なければ0を返す（簡易実装）
        if let Some(expr) = &block.return_expr {
            self.eval_expr(expr, env)
        } else {
            0
        }
    }

    fn eval_expr(&self, expr: &parser::Expr, env: &mut Environment) -> i64 {
        match expr {
            parser::Expr::Number(n) => *n,
            parser::Expr::Variable(name) => *env.variables.get(name).expect("Undefined variable"),
            parser::Expr::Binary { left, op, right } => {
                let l = self.eval_expr(left, env);
                let r = self.eval_expr(right, env);
                match op {
                    parser::BinOp::Add => l + r,
                    parser::BinOp::Sub => l - r,
                    parser::BinOp::Mul => l * r,
                    parser::BinOp::Div => l / r,
                }
            }
            parser::Expr::Assign { name, value } => {
                let val = self.eval_expr(value, env);
                if env.variables.contains_key(name) {
                    env.variables.insert(name.clone(), val);
                    val
                } else {
                    panic!("Variable '{}' not defined", name);
                }
            }
            parser::Expr::Call { name, args } => {
                if name == "print" {
                    for arg in args {
                        let val = self.eval_expr(arg, env);
                        println!("{}", val);
                    }
                    return 0;
                } else if name == "ask" {
                    assert!(args.len() == 0);
                    let mut input = String::new();
                    std::io::stdin().read_line(&mut input).unwrap();
                    return input.trim().parse().unwrap();
                }
                let func = self.functions.get(name).expect("Undefined function");
                let mut new_env = Environment::default();

                // 引数を評価して新しい環境（スコープ）にバインド
                for (param_name, arg_expr) in func.params.iter().zip(args) {
                    let val = self.eval_expr(arg_expr, env);
                    new_env.variables.insert(param_name.clone(), val);
                }

                // 関数のボディを実行
                self.execute_block(&func.body, &mut new_env)
            }
        }
    }
}