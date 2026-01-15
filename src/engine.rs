use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;
use std::fmt;
use crate::ast::*;

// ==========================================
// 1. Runtime Values
// ==========================================

#[derive(Debug, Clone)]
pub enum Value {
    Int(i64),
    Float(f64),
    String(String),
    Bool(bool),
    Object(Rc<RefCell<HashMap<String, Value>>>), // オブジェクトは可変マップとして表現
    Function {
        name: String,
        params: Vec<String>,
        body: Block,
        env: Rc<RefCell<Environment>>, // クロージャ (定義時の環境を保持)
    },
    NativeFunction(fn(Vec<Value>) -> Value), // printなどの組み込み用
    Null,
    // Control Flow Signals (internal use only)
    Return(Box<Value>),
    Break,
    Continue,
}

impl fmt::Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Value::Int(v) => write!(f, "{}", v),
            Value::Float(v) => write!(f, "{}", v),
            Value::String(v) => write!(f, "{}", v),
            Value::Bool(v) => write!(f, "{}", v),
            Value::Object(_) => write!(f, "<Object>"),
            Value::Function { name, .. } => write!(f, "<Function {}>", name),
            Value::NativeFunction(_) => write!(f, "<NativeFunction>"),
            Value::Null => write!(f, "null"),
            _ => write!(f, "<ControlFlow>"),
        }
    }
}

// Helper to extract boolean from Value
impl Value {
    pub fn is_truthy(&self) -> bool {
        match self {
            Value::Bool(b) => *b,
            Value::Int(i) => *i != 0,
            Value::Null => false,
            _ => true,
        }
    }
}

// ==========================================
// 2. Environment (Scope)
// ==========================================

#[derive(Debug, Clone)]
pub struct Environment {
    pub values: HashMap<String, Value>,
    pub parent: Option<Rc<RefCell<Environment>>>,
}

impl Environment {
    pub fn new(parent: Option<Rc<RefCell<Environment>>>) -> Self {
        Self {
            values: HashMap::new(),
            parent,
        }
    }

    pub fn define(&mut self, name: String, value: Value) {
        self.values.insert(name, value);
    }

    pub fn get(&self, name: &str) -> Option<Value> {
        if let Some(val) = self.values.get(name) {
            Some(val.clone())
        } else if let Some(parent) = &self.parent {
            parent.borrow().get(name)
        } else {
            None
        }
    }

    // 代入（既存の変数を更新）EBNF上は let のみだが、論理的に再代入が必要な場合に使用
    pub fn assign(&mut self, name: &str, value: Value) -> Result<(), String> {
        if self.values.contains_key(name) {
            self.values.insert(name.to_string(), value);
            Ok(())
        } else if let Some(parent) = &self.parent {
            parent.borrow_mut().assign(name, value)
        } else {
            Err(format!("Undefined variable '{}'", name))
        }
    }
}

// ==========================================
// 3. Interpreter Engine
// ==========================================

pub struct Interpreter {
    pub global_env: Rc<RefCell<Environment>>,
}

impl Interpreter {
    pub fn new() -> Self {
        let env = Rc::new(RefCell::new(Environment::new(None)));
        
        // Native "print" function
        env.borrow_mut().define("print".to_string(), Value::NativeFunction(|args| {
            let output = args.iter().map(|v| format!("{}", v)).collect::<Vec<_>>().join(" ");
            println!("{}", output);
            Value::Null
        }));

        Self { global_env: env }
    }

    pub fn eval_program(&mut self, program: &Program) -> Result<(), String> {
        // 1. Register all functions and objects first (hoisting-like behavior)
        for decl in &program.decls {
            match decl {
                Decl::Function { name, params, body } => {
                    let func_val = Value::Function {
                        name: name.clone(),
                        params: params.iter().map(|p| p.name.clone()).collect(),
                        body: body.clone(),
                        env: Rc::clone(&self.global_env),
                    };
                    self.global_env.borrow_mut().define(name.clone(), func_val);
                },
                Decl::Object { name, .. } => {
                    // EBNFではObject宣言は構造定義に近いですが、
                    // 簡易的に空のオブジェクト作成機能として登録、またはプレースホルダーとします。
                    // ここでは「型名」としては何もしませんが、同名の変数として空Objを入れておきます。
                    // 本来は `Point::new` 等をするなら static メソッドが必要ですが、
                    // EBNFにimpl定義がないため、モックとして扱います。
                    let obj_val = Value::Object(Rc::new(RefCell::new(HashMap::new())));
                    self.global_env.borrow_mut().define(name.clone(), obj_val);
                }
            }
        }

        // 2. Execute "main" function if exists
        let main = self.global_env.borrow().get("main");
        if let Some(Value::Function { body, env, .. }) = main {
            // main関数は引数なしと仮定
            let new_env = Environment::new(Some(env)); // Close over declaration env
            self.eval_block(&body, Rc::new(RefCell::new(new_env)))?;
        } else {
            return Err("No main function found.".to_string());
        }

        Ok(())
    }

    fn eval_block(&mut self, block: &Block, env: Rc<RefCell<Environment>>) -> Result<Value, String> {
        for stmt in &block.statements {
            let result = self.eval_statement(stmt, Rc::clone(&env))?;
            // Handle Control Flow
            match result {
                Value::Return(_) | Value::Break | Value::Continue => return Ok(result),
                _ => {}
            }
        }
        Ok(Value::Null)
    }

    fn eval_statement(&mut self, stmt: &Statement, env: Rc<RefCell<Environment>>) -> Result<Value, String> {
        match stmt {
            Statement::Return(expr) => {
                let val = self.eval_expression(expr, env)?;
                Ok(Value::Return(Box::new(val)))
            },
            Statement::Let { name, init } => {
                let val = if let Some(expr) = init {
                    self.eval_expression(expr, Rc::clone(&env))?
                } else {
                    Value::Null
                };
                env.borrow_mut().define(name.clone(), val);
                Ok(Value::Null)
            },
            Statement::Expression(expr) => {
                self.eval_expression(expr, env)?;
                Ok(Value::Null)
            },
            Statement::If { condition, then_block, else_ifs, else_block } => {
                let cond_val = self.eval_expression(condition, Rc::clone(&env))?;
                if cond_val.is_truthy() {
                    let scope = Rc::new(RefCell::new(Environment::new(Some(Rc::clone(&env)))));
                    return self.eval_block(then_block, scope);
                }

                for (cond_expr, block) in else_ifs {
                    let c_val = self.eval_expression(cond_expr, Rc::clone(&env))?;
                    if c_val.is_truthy() {
                        let scope = Rc::new(RefCell::new(Environment::new(Some(Rc::clone(&env)))));
                        return self.eval_block(block, scope);
                    }
                }

                if let Some(block) = else_block {
                    let scope = Rc::new(RefCell::new(Environment::new(Some(Rc::clone(&env)))));
                    return self.eval_block(block, scope);
                }
                Ok(Value::Null)
            },
            Statement::While { condition, block } => {
                loop {
                    let cond_val = self.eval_expression(condition, Rc::clone(&env))?;
                    if !cond_val.is_truthy() {
                        break;
                    }
                    let scope = Rc::new(RefCell::new(Environment::new(Some(Rc::clone(&env)))));
                    let res = self.eval_block(block, scope)?;
                    match res {
                        Value::Break => break,
                        Value::Continue => continue,
                        Value::Return(v) => return Ok(Value::Return(v)),
                        _ => {}
                    }
                }
                Ok(Value::Null)
            },
            Statement::Break => Ok(Value::Break),
            Statement::Continue => Ok(Value::Continue),
        }
    }

    fn eval_expression(&mut self, expr: &Expression, env: Rc<RefCell<Environment>>) -> Result<Value, String> {
        match expr {
            Expression::Literal(lit) => match lit {
                Literal::Number(s) => {
                    // Try parsing as Int, then Float
                    if let Ok(i) = s.parse::<i64>() {
                        Ok(Value::Int(i))
                    } else {
                        Ok(Value::Float(s.parse::<f64>().unwrap_or(0.0)))
                    }
                },
                Literal::String(s) => Ok(Value::String(s.clone())),
                Literal::Bool(b) => Ok(Value::Bool(*b)),
            },
            Expression::Identifier(name) => {
                env.borrow().get(name).ok_or_else(|| format!("Variable not found: {}", name))
            },
            Expression::Binary { left, op, right } => {
                let l = self.eval_expression(left, Rc::clone(&env))?;
                let r = self.eval_expression(right, Rc::clone(&env))?;
                self.eval_binary_op(l, op, r)
            },
            Expression::Unary { op, expr } => {
                let val = self.eval_expression(expr, env)?;
                match op {
                    UnaryOp::Neg => match val {
                        Value::Int(i) => Ok(Value::Int(-i)),
                        Value::Float(f) => Ok(Value::Float(-f)),
                        _ => Err("Invalid type for negation".to_string()),
                    },
                    UnaryOp::Not => Ok(Value::Bool(!val.is_truthy())),
                }
            },
            Expression::Call { callee, args } => {
                let func = self.eval_expression(callee, Rc::clone(&env))?;
                let mut arg_vals = Vec::new();
                for arg in args {
                    arg_vals.push(self.eval_expression(arg, Rc::clone(&env))?);
                }

                match func {
                    Value::Function { params, body, env: closure_env, .. } => {
                        if params.len() != arg_vals.len() {
                            return Err(format!("Arg count mismatch. Expected {}, got {}", params.len(), arg_vals.len()));
                        }
                        // Create scope for function execution
                        let scope = Rc::new(RefCell::new(Environment::new(Some(closure_env))));
                        for (name, val) in params.iter().zip(arg_vals.into_iter()) {
                            scope.borrow_mut().define(name.clone(), val);
                        }
                        
                        let result = self.eval_block(&body, scope)?;
                        if let Value::Return(v) = result {
                            Ok(*v)
                        } else {
                            Ok(result)
                        }
                    },
                    Value::NativeFunction(f) => Ok(f(arg_vals)),
                    _ => Err("Trying to call a non-function".to_string()),
                }
            },
            Expression::Access { target, field, separator: _ } => {
                // EBNFの定義上、Accessは `expr.id` または `expr::id`
                let target_val = self.eval_expression(target, env)?;
                match target_val {
                    Value::Object(map) => {
                        let m = map.borrow();
                        if let Some(val) = m.get(field) {
                            Ok(val.clone())
                        } else {
                            // 動的にフィールドが見つからない場合、Nullを返すかエラーにする
                            // 今回はエラーにする
                            Err(format!("Field '{}' not found on object", field))
                        }
                    },
                    _ => Err(format!("Cannot access property '{}' on non-object", field)),
                }
            }
        }
    }

    fn eval_binary_op(&self, left: Value, op: &BinaryOp, right: Value) -> Result<Value, String> {
        // 簡易実装: 型強制や細かいエラー処理は省略
        match (left, right) {
            (Value::Int(l), Value::Int(r)) => match op {
                BinaryOp::Add => Ok(Value::Int(l + r)),
                BinaryOp::Sub => Ok(Value::Int(l - r)),
                BinaryOp::Mul => Ok(Value::Int(l * r)),
                BinaryOp::Div => Ok(Value::Int(l / r)),
                BinaryOp::Eq => Ok(Value::Bool(l == r)),
                BinaryOp::Ne => Ok(Value::Bool(l != r)),
                BinaryOp::Lt => Ok(Value::Bool(l < r)),
                BinaryOp::Gt => Ok(Value::Bool(l > r)),
                BinaryOp::Le => Ok(Value::Bool(l <= r)),
                BinaryOp::Ge => Ok(Value::Bool(l >= r)),
                _ => Err("Invalid op for Int".to_string()),
            },
            (Value::Bool(l), Value::Bool(r)) => match op {
                BinaryOp::And => Ok(Value::Bool(l && r)),
                BinaryOp::Or => Ok(Value::Bool(l || r)),
                BinaryOp::Eq => Ok(Value::Bool(l == r)),
                BinaryOp::Ne => Ok(Value::Bool(l != r)),
                _ => Err("Invalid op for Bool".to_string()),
            },
            // String concat
            (Value::String(l), Value::String(r)) => match op {
                BinaryOp::Add => Ok(Value::String(l + &r)),
                BinaryOp::Eq => Ok(Value::Bool(l == r)),
                BinaryOp::Ne => Ok(Value::Bool(l != r)),
                _ => Err("Invalid op for String".to_string()),
            },
            // Fallback for mixed types or other ops (panic or error)
            (l, r) => Err(format!("Unsupported binary op {:?} between {} and {}", op, l, r)),
        }
    }
}
