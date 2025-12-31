use std::collections::HashMap;
use super::parser;

/// 変数や関数のスコープを管理する構造体
#[derive(Clone)]
pub struct Environment {
    variables: HashMap<String, Value>,
}

impl Environment {
    pub fn new() -> Self {
        Self {
            variables: HashMap::new(),
        }
    }
}

pub struct Interpreter {
    functions: HashMap<String, parser::FunctionDecl>,
}

#[derive(Clone, Debug)]
pub enum Value {
    Number(f64),
    String(String),
    Bool(bool),
    Null,
}

impl std::ops::Add for Value {
    type Output = Value;
    fn add(self, rhs: Self) -> Self::Output {
        let rhs = &rhs;
        let this = &self;
        match (this, rhs) {
            (Value::Number(l), Value::Number(r)) => Value::Number(l + r),
            (Value::String(l), Value::String(r)) => Value::String(format!("{}{}", l, r)),
            _ => panic!("Cannot add {:?} and {:?}", this, rhs),
        }
    }
}

impl std::ops::Sub for Value {
    type Output = Value;
    fn sub(self, rhs: Self) -> Self::Output {
        let rhs = &rhs;
        let this = &self;
        match (this, rhs) {
            (Value::Number(l), Value::Number(r)) => Value::Number(l - r),
            _ => panic!("Cannot add {:?} and {:?}", this, rhs),
        }
    }
}

impl std::ops::Mul for Value {
    type Output = Value;
    fn mul(self, rhs: Self) -> Self::Output {
        let rhs = &rhs;
        let this = &self;
        match (this, rhs) {
            (Value::Number(l), Value::Number(r)) => Value::Number(l * r),
            _ => panic!("Cannot add {:?} and {:?}", this, rhs),
        }
    }
}

impl std::ops::Div for Value {
    type Output = Value;
    fn div(self, rhs: Self) -> Self::Output {
        let rhs = &rhs;
        let this = &self;
        match (rhs, this) {
            (Value::Number(_), Value::Number(0f64)) => panic!("Cannot divide by zero"),
            (Value::Number(l), Value::Number(r)) => Value::Number(l / r),
            _ => panic!("Cannot add {:?} and {:?}", this, rhs),
        }
    }
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
    pub fn run(&self) -> Value {
        let main_fn = self.functions.get("main").expect("main function not found");
        let mut env = Environment::new();
        self.execute_block(&main_fn.body, &mut env)
    }

    fn execute_block(&self, block: &parser::Block, env: &mut Environment) -> Value {
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
            Value::Null
        }
    }

    fn eval_expr(&self, expr: &parser::Expr, env: &mut Environment) -> Value {
        match expr {
            parser::Expr::Number(n) => Value::Number(*n),
            parser::Expr::Variable(name) => env.variables.get(name).expect("Undefined variable").clone(),
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
                    env.variables.insert(name.clone(), val.clone());
                    val
                } else {
                    panic!("Variable '{}' not defined", name);
                }
            }
            parser::Expr::Call { name, args } => {
                if name == "print" {
                    for arg in args {
                        let val = self.eval_expr(arg, env);
                        match val {
                            Value::Number(n) => println!("{}", n),
                            Value::String(s) => println!("{}", s),
                            Value::Bool(b) => println!("{}", b),
                            Value::Null => println!("null"),
                        }
                    }
                    return Value::Null;
                } else if name == "ask" {
                    assert!(args.len() == 0);
                    let mut input = String::new();
                    std::io::stdin().read_line(&mut input).unwrap();
                    return Value::String(input)
                }
                let func = self.functions.get(name).expect("Undefined function");
                let mut new_env = Environment::new();

                // 引数を評価して新しい環境（スコープ）にバインド
                for (param_name, arg_expr) in func.params.iter().zip(args) {
                    let val = self.eval_expr(arg_expr, env);
                    new_env.variables.insert(param_name.clone(), val);
                }

                // 関数のボディを実行
                self.execute_block(&func.body, &mut new_env)
            }
            parser::Expr::String(s) => Value::String(s.to_string()),
            &parser::Expr::Bool(b) => Value::Bool(b),
            
        }
    }
}