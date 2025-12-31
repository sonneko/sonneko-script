use super::lexer::Token;

#[derive(Debug, PartialEq, Clone)]
pub enum Expr {
    // 数値リテラル: 42
    Number(f64),
    String(String),
    Bool(bool),
    Null,
    
    // 変数参照: x
    Variable(String),
    
    // 二項演算: 1 + 2, x * 10
    Binary {
        left: Box<Expr>,
        op: BinOp,
        right: Box<Expr>,
    },
    
    // 関数呼び出し: add(1, 2)
    Call {
        name: String,
        args: Vec<Expr>,
    },
    
    // 変数代入: x = 5
    Assign {
        name: String,
        value: Box<Expr>,
    },

    If {
        condition: Box<Expr>,
        then_branch: Block,
        else_branch: Option<Block>,
    },
}

#[derive(Debug, PartialEq, Clone)]
pub enum BinOp {
    Add,      // +
    Sub,      // -
    Mul,      // *
    Div,      // /
}

#[derive(Debug, PartialEq, Clone)]
pub enum Statement {
    // 変数宣言: let x = 5;
    Let {
        name: String,
        value: Option<Expr>,
    },
    // 式文: foo();
    ExprStmt(Expr),
}

#[derive(Debug, PartialEq, Clone)]
pub struct Block {
    pub statements: Vec<Statement>,
    pub return_expr: Option<Box<Expr>>, // Rust風に最後がセミコロンなしなら戻り値
}

#[derive(Debug, PartialEq, Clone)]
pub struct FunctionDecl {
    pub name: String,
    pub params: Vec<String>,
    pub body: Block,
}

#[derive(Debug, PartialEq, Clone)]
pub struct Program {
    pub functions: Vec<FunctionDecl>,
}

pub struct Parser {
    tokens: Vec<Token>,
    pos: usize,
}

impl Parser {
    pub fn new(tokens: Vec<Token>) -> Self {
        Self { tokens, pos: 0 }
    }

    // ヘルパー関数: 現在のトークンを取得
    fn peek(&self) -> &Token {
        &self.tokens[self.pos]
    }

    // ヘルパー関数: トークンを1つ進める
    fn advance(&mut self) -> Token {
        let token = self.tokens[self.pos].clone();
        if token != Token::EOF {
            self.pos += 1;
        }
        token
    }

    // ヘルパー関数: 期待したトークンがあれば消費、なければエラー
    fn expect(&mut self, expected: Token) {
        let actual = self.advance();
        if actual != expected {
            panic!("Expected {:?}, but found {:?}", expected, actual);
        }
    }

    // Program = { FunctionDecl }
    pub fn parse_program(&mut self) -> Program {
        let mut functions = Vec::new();
        while *self.peek() != Token::EOF {
            functions.push(self.parse_function_decl());
        }
        Program { functions }
    }

    // FunctionDecl = "fn" , Identifier , "(" , [ ParamList ] , ")" , Block
    fn parse_function_decl(&mut self) -> FunctionDecl {
        self.expect(Token::Fn);
        
        let name = if let Token::Identifier(id) = self.advance() { id } else { panic!("Expected function name") };
        
        self.expect(Token::LParen);
        let mut params = Vec::new();
        if let Token::Identifier(_) = self.peek() {
            if let Token::Identifier(id) = self.advance() { params.push(id); }
            while *self.peek() == Token::Comma {
                self.advance();
                if let Token::Identifier(id) = self.advance() { params.push(id); }
            }
        }
        self.expect(Token::RParen);
        
        let body = self.parse_block();
        FunctionDecl { name, params, body }
    }

    // Block = "{" , { Statement } , [ Expression ] , "}"
    fn parse_block(&mut self) -> Block {
        self.expect(Token::LBrace);
        let mut statements = Vec::new();
        let mut return_expr = None;

        while *self.peek() != Token::RBrace {
            if *self.peek() == Token::Let {
                statements.push(self.parse_let_stmt());
            } else {
                let expr = self.parse_expression();
                if *self.peek() == Token::SemiColon {
                    self.advance();
                    statements.push(Statement::ExprStmt(expr));
                } else {
                    return_expr = Some(Box::new(expr));
                    break;
                }
            }
        }
        self.expect(Token::RBrace);
        Block { statements, return_expr }
    }

    fn parse_let_stmt(&mut self) -> Statement {
        self.expect(Token::Let);
        let name = if let Token::Identifier(id) = self.advance() { id } else { panic!("Expected variable name") };
        if let Token::SemiColon = self.peek() {
            self.advance();
            return Statement::Let { name, value: None };
        }
        self.expect(Token::Assign);
        let value = self.parse_expression();
        self.expect(Token::SemiColon);
        Statement::Let { name, value: Some(value) }
    }

    // Expression = Assignment
    fn parse_expression(&mut self) -> Expr {
        self.parse_assignment()
    }

    // Assignment = Identifier , "=" , Expression | Addition | "if" , Expression , Block, "else" , Expression
    fn parse_assignment(&mut self) -> Expr {
        if self.peek() == &Token::If {
            self.advance();
            let condition = self.parse_expression();
            let then_branch = self.parse_block();
            if self.peek() == &Token::Else {
                self.advance();
                let else_branch = self.parse_block();
                return Expr::If { condition: Box::new(condition), then_branch, else_branch: Some(else_branch) };
            }
            return Expr::If { condition: Box::new(condition), then_branch, else_branch: None };
        }
        let left = self.parse_addition();
        
        if let Expr::Variable(name) = &left {
            if *self.peek() == Token::Assign {
                self.advance();
                let value = self.parse_expression();
                return Expr::Assign { name: name.clone(), value: Box::new(value) };
            }
        }
        left
    }

    // Addition = Multiplication , { ( "+" | "-" ) , Multiplication }
    fn parse_addition(&mut self) -> Expr {
        let mut node = self.parse_multiplication();
        while matches!(self.peek(), Token::Plus | Token::Minus) {
            let op = if self.advance() == Token::Plus { BinOp::Add } else { BinOp::Sub };
            let right = self.parse_multiplication();
            node = Expr::Binary { left: Box::new(node), op, right: Box::new(right) };
        }
        node
    }

    // Multiplication = Primary , { ( "*" | "/" ) , Primary }
    fn parse_multiplication(&mut self) -> Expr {
        let mut node = self.parse_primary();
        while matches!(self.peek(), Token::Asterisk | Token::Slash) {
            let op = if self.advance() == Token::Asterisk { BinOp::Mul } else { BinOp::Div };
            let right = self.parse_primary();
            node = Expr::Binary { left: Box::new(node), op, right: Box::new(right) };
        }
        node
    }

    // Primary = Number | String | Bool | FunctionCall | Identifier | "(" , Expression , ")"
    fn parse_primary(&mut self) -> Expr {
        match self.advance() {
            Token::Number(n) => Expr::Number(n),
            Token::LParen => {
                let expr = self.parse_expression();
                self.expect(Token::RParen);
                expr
            }
            Token::Identifier(id) => {
                if *self.peek() == Token::LParen {
                    self.advance();
                    let mut args = Vec::new();
                    if *self.peek() != Token::RParen {
                        args.push(self.parse_expression());
                        while *self.peek() == Token::Comma {
                            self.advance();
                            args.push(self.parse_expression());
                        }
                    }
                    self.expect(Token::RParen);
                    Expr::Call { name: id, args }
                } else {
                    Expr::Variable(id)
                }
            }
            Token::Bool(b) => Expr::Bool(b),
            Token::String(s) => Expr::String(s),
            _ => panic!("Unexpected token"),
        }
    }
}