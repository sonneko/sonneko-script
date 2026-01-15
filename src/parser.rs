use crate::tokens::{Token, TokenKind};
use crate::ast::*;
use std::iter::Peekable;
use std::vec::IntoIter;

// ==========================================
// 4. Parser (Recursive Descent)
// ==========================================

pub struct Parser {
    tokens: Peekable<IntoIter<Token>>,
}

impl Parser {
    pub fn new(tokens: Vec<Token>) -> Self {
        Self {
            tokens: tokens.into_iter().peekable(),
        }
    }

    // --- Helper Methods ---

    fn peek(&mut self) -> TokenKind {
        self.tokens.peek().map(|t| t.kind.clone()).unwrap_or(TokenKind::EOF)
    }

    fn advance(&mut self) -> Token {
        self.tokens.next().unwrap_or(Token { kind: TokenKind::EOF })
    }

    fn consume(&mut self, expected: TokenKind) -> Result<(), String> {
        let got = self.peek();
        if got == expected {
            self.advance();
            Ok(())
        } else {
            Err(format!("Expected {:?}, but got {:?}", expected, got))
        }
    }

    fn expect_identifier(&mut self) -> Result<String, String> {
        match self.peek() {
            TokenKind::Identifier(s) => {
                self.advance();
                Ok(s)
            }
            got => Err(format!("Expected Identifier, got {:?}", got)),
        }
    }

    // --- Grammar Implementation ---

    // <program> ::= { <function_decl> | <object_decl> }
    pub fn parse_program(&mut self) -> Result<Program, String> {
        let mut decls = Vec::new();
        while self.peek() != TokenKind::EOF {
            match self.peek() {
                TokenKind::Fn => decls.push(self.parse_function_decl()?),
                TokenKind::Obj => decls.push(self.parse_object_decl()?),
                got => return Err(format!("Expected 'fn' or 'obj', got {:?}", got)),
            }
        }
        Ok(Program { decls })
    }

    // <function_decl> ::= "fn" <identifier> "(" [<param_list>] ")" <block>
    fn parse_function_decl(&mut self) -> Result<Decl, String> {
        self.consume(TokenKind::Fn)?;
        let name = self.expect_identifier()?;
        self.consume(TokenKind::LParen)?;
        
        let mut params = Vec::new();
        if self.peek() != TokenKind::RParen {
            params = self.parse_param_list()?;
        }
        
        self.consume(TokenKind::RParen)?;
        let body = self.parse_block()?;
        
        Ok(Decl::Function { name, params, body })
    }

    // <object_decl> ::= "obj" <identifier> "=" <obj_literal>
    fn parse_object_decl(&mut self) -> Result<Decl, String> {
        self.consume(TokenKind::Obj)?;
        let name = self.expect_identifier()?;
        self.consume(TokenKind::Assign)?;
        let literals = self.parse_obj_literal()?; // Returns Vec<Param>
        Ok(Decl::Object { name, literals })
    }

    // <obj_literal> ::= "{" [<param_list>] "}"
    fn parse_obj_literal(&mut self) -> Result<Vec<Param>, String> {
        self.consume(TokenKind::LBrace)?;
        let mut params = Vec::new();
        if self.peek() != TokenKind::RBrace {
            params = self.parse_param_list()?;
        }
        self.consume(TokenKind::RBrace)?;
        Ok(params)
    }

    // <param_list> ::= <param> {"," <param>}
    fn parse_param_list(&mut self) -> Result<Vec<Param>, String> {
        let mut params = Vec::new();
        params.push(self.parse_param()?);
        
        while self.peek() == TokenKind::Comma {
            self.advance();
            params.push(self.parse_param()?);
        }
        Ok(params)
    }

    // <param> ::= <identifier> ":" <type_literal>
    fn parse_param(&mut self) -> Result<Param, String> {
        let name = self.expect_identifier()?;
        self.consume(TokenKind::Colon)?;
        let type_lit = self.parse_type_literal()?;
        Ok(Param { name, type_lit })
    }

    // <type_literal> ::= "Int" | "Float" | "String" | "Bool" | <identifier>
    fn parse_type_literal(&mut self) -> Result<TypeLiteral, String> {
        match self.peek() {
            TokenKind::TypeInt => { self.advance(); Ok(TypeLiteral::Int) },
            TokenKind::TypeFloat => { self.advance(); Ok(TypeLiteral::Float) },
            TokenKind::TypeString => { self.advance(); Ok(TypeLiteral::String) },
            TokenKind::TypeBool => { self.advance(); Ok(TypeLiteral::Bool) },
            TokenKind::Identifier(s) => { self.advance(); Ok(TypeLiteral::Custom(s)) },
            got => Err(format!("Expected type literal, got {:?}", got)),
        }
    }

    // <block> ::= "{" {<statement>} "}"
    fn parse_block(&mut self) -> Result<Block, String> {
        self.consume(TokenKind::LBrace)?;
        let mut statements = Vec::new();
        while self.peek() != TokenKind::RBrace && self.peek() != TokenKind::EOF {
            statements.push(self.parse_statement()?);
        }
        self.consume(TokenKind::RBrace)?;
        Ok(Block { statements })
    }

    // <statement> select based on start token
    fn parse_statement(&mut self) -> Result<Statement, String> {
        match self.peek() {
            TokenKind::Return => self.parse_return_statement(),
            TokenKind::Let => self.parse_let_statement(),
            TokenKind::If => self.parse_if_statement(),
            TokenKind::While => self.parse_while_statement(),
            TokenKind::Break => self.parse_break_statement(),
            TokenKind::Continue => self.parse_continue_statement(),
            _ => self.parse_expression_statement(),
        }
    }

    // <return_statement> ::= "return" <expression> ";"
    fn parse_return_statement(&mut self) -> Result<Statement, String> {
        self.consume(TokenKind::Return)?;
        let expr = self.parse_expression()?;
        self.consume(TokenKind::SemiColon)?;
        Ok(Statement::Return(expr))
    }

    // <let_statement> ::= "let" <identifier> [ "=" <expression> ] ";"
    fn parse_let_statement(&mut self) -> Result<Statement, String> {
        self.consume(TokenKind::Let)?;
        let name = self.expect_identifier()?;
        let mut init = None;
        if self.peek() == TokenKind::Assign {
            self.advance();
            init = Some(self.parse_expression()?);
        }
        self.consume(TokenKind::SemiColon)?;
        Ok(Statement::Let { name, init })
    }

    // <expression_statement> ::= <expression> ";"
    fn parse_expression_statement(&mut self) -> Result<Statement, String> {
        let expr = self.parse_expression()?;
        self.consume(TokenKind::SemiColon)?;
        Ok(Statement::Expression(expr))
    }

    // <if_statement>
    fn parse_if_statement(&mut self) -> Result<Statement, String> {
        self.consume(TokenKind::If)?;
        let condition = self.parse_expression()?;
        let then_block = self.parse_block()?;
        
        let mut else_ifs = Vec::new();
        let mut else_block = None;

        while self.peek() == TokenKind::Else {
            self.advance(); // consume 'else'
            if self.peek() == TokenKind::If {
                self.advance(); // consume 'if'
                let ei_cond = self.parse_expression()?;
                let ei_block = self.parse_block()?;
                else_ifs.push((ei_cond, ei_block));
            } else {
                // just 'else' -> else block
                else_block = Some(self.parse_block()?);
                break;
            }
        }

        Ok(Statement::If { condition, then_block, else_ifs, else_block })
    }

    // <while_statement> ::= "while" <expression> <block>
    fn parse_while_statement(&mut self) -> Result<Statement, String> {
        self.consume(TokenKind::While)?;
        let condition = self.parse_expression()?;
        let block = self.parse_block()?;
        Ok(Statement::While { condition, block })
    }

    // <break_statement> ::= "break" ";"
    fn parse_break_statement(&mut self) -> Result<Statement, String> {
        self.consume(TokenKind::Break)?;
        self.consume(TokenKind::SemiColon)?;
        Ok(Statement::Break)
    }

    // <continue_statement> ::= "continue" ";"
    fn parse_continue_statement(&mut self) -> Result<Statement, String> {
        self.consume(TokenKind::Continue)?;
        self.consume(TokenKind::SemiColon)?;
        Ok(Statement::Continue)
    }

    // Expressions
    
    // <expression> ::= <logical_or> { "||" <logical_or> }
    fn parse_expression(&mut self) -> Result<Expression, String> {
        let mut left = self.parse_logical_or()?;
        while self.peek() == TokenKind::Or {
            self.advance();
            let right = self.parse_logical_or()?;
            left = Expression::Binary { left: Box::new(left), op: BinaryOp::Or, right: Box::new(right) };
        }
        Ok(left)
    }

    // <logical_or> ::= <logical_and> { "&&" <logical_and> }
    fn parse_logical_or(&mut self) -> Result<Expression, String> {
        let mut left = self.parse_logical_and()?;
        while self.peek() == TokenKind::And {
            self.advance();
            let right = self.parse_logical_and()?;
            left = Expression::Binary { left: Box::new(left), op: BinaryOp::And, right: Box::new(right) };
        }
        Ok(left)
    }

    // <logical_and_expression> ::= <equality_expression> {("==" | "!=") <equality_expression>}
    fn parse_logical_and(&mut self) -> Result<Expression, String> {
        let mut left = self.parse_equality()?;
        // EBNF strictly implies NO loop, but just one op. 
        // We use a loop for standard precedence climbing behavior (left associative).
        while matches!(self.peek(), TokenKind::Eq | TokenKind::Ne) {
            let op = match self.advance().kind {
                TokenKind::Eq => BinaryOp::Eq,
                TokenKind::Ne => BinaryOp::Ne,
                _ => unreachable!(),
            };
            let right = self.parse_equality()?;
            left = Expression::Binary { left: Box::new(left), op, right: Box::new(right) };
        }
        Ok(left)
    }

    // <equality_expression> ::= <relational> ...
    fn parse_equality(&mut self) -> Result<Expression, String> {
        let mut left = self.parse_relational()?;
        while matches!(self.peek(), TokenKind::Lt | TokenKind::Le | TokenKind::Gt | TokenKind::Ge) {
            let op = match self.advance().kind {
                TokenKind::Lt => BinaryOp::Lt,
                TokenKind::Le => BinaryOp::Le,
                TokenKind::Gt => BinaryOp::Gt,
                TokenKind::Ge => BinaryOp::Ge,
                _ => unreachable!(),
            };
            let right = self.parse_relational()?;
            left = Expression::Binary { left: Box::new(left), op, right: Box::new(right) };
        }
        Ok(left)
    }

    // <relational_expression> ::= <additive> ...
    fn parse_relational(&mut self) -> Result<Expression, String> {
        let mut left = self.parse_additive()?;
        while matches!(self.peek(), TokenKind::Plus | TokenKind::Minus) {
            let op = match self.advance().kind {
                TokenKind::Plus => BinaryOp::Add,
                TokenKind::Minus => BinaryOp::Sub,
                _ => unreachable!(),
            };
            let right = self.parse_additive()?;
            left = Expression::Binary { left: Box::new(left), op, right: Box::new(right) };
        }
        Ok(left)
    }

    // <additive_expression> ::= <multiplicative> ...
    fn parse_additive(&mut self) -> Result<Expression, String> {
        let mut left = self.parse_multiplicative()?;
        while matches!(self.peek(), TokenKind::Star | TokenKind::Slash) {
            let op = match self.advance().kind {
                TokenKind::Star => BinaryOp::Mul,
                TokenKind::Slash => BinaryOp::Div,
                _ => unreachable!(),
            };
            let right = self.parse_multiplicative()?;
            left = Expression::Binary { left: Box::new(left), op, right: Box::new(right) };
        }
        Ok(left)
    }

    // <multiplicative_expression> ::= ["-" | "!"] <unary_expression>
    fn parse_multiplicative(&mut self) -> Result<Expression, String> {
        if matches!(self.peek(), TokenKind::Minus | TokenKind::Not) {
            let op_token = self.advance();
            let op = if op_token.kind == TokenKind::Minus { UnaryOp::Neg } else { UnaryOp::Not };
            let expr = self.parse_unary()?;
            Ok(Expression::Unary { op, expr: Box::new(expr) })
        } else {
            self.parse_unary()
        }
    }

    // <unary_expression> ::= "(" <expression> ")" | <primary_expression>
    // However, <primary_expression> includes <accesser> which includes <expression>.
    // Left recursion fix: We start with atoms (paren-expr, literals, identifiers)
    // and then apply postfix operations (accessors, calls).
    fn parse_unary(&mut self) -> Result<Expression, String> {
        // Base case: Atom
        let mut expr = if self.peek() == TokenKind::LParen {
            self.advance();
            let e = self.parse_expression()?;
            self.consume(TokenKind::RParen)?;
            e
        } else {
            self.parse_primary_atom()?
        };

        // Postfix loop: handles <accesser> and <function_call> logic
        // <accesser> ::= <expression> {("::" | ".") <identifier>}
        // <function_call> ::= <accesser> "(" [<argument_list>] ")"
        loop {
            match self.peek() {
                TokenKind::Dot | TokenKind::DoubleColon => {
                    // Accesser logic
                    let token = self.advance();
                    let separator = if token.kind == TokenKind::Dot { ".".to_string() } else { "::".to_string() };
                    let field = self.expect_identifier()?;
                    expr = Expression::Access {
                        target: Box::new(expr),
                        separator,
                        field,
                    };
                },
                TokenKind::LParen => {
                    // Function Call logic
                    // EBNF: <function_call> ::= <accesser> "(" [<argument_list>] ")"
                    // Since 'expr' currently holds the <accesser> part (or a simple atom), we apply the call.
                    self.advance(); // consume '('
                    let mut args = Vec::new();
                    if self.peek() != TokenKind::RParen {
                        args = self.parse_argument_list()?;
                    }
                    self.consume(TokenKind::RParen)?;
                    expr = Expression::Call {
                        callee: Box::new(expr),
                        args
                    };
                },
                _ => break,
            }
        }

        Ok(expr)
    }

    // Helper to parse the non-recursive parts of <primary_expression>
    // <primary_expression> ::= <function_call> | <literal> | <accesser> | <identifier>
    // Function call and Accesser are handled in parse_unary via postfix loop.
    fn parse_primary_atom(&mut self) -> Result<Expression, String> {
        match self.peek() {
            TokenKind::NumberLit(n) => {
                self.advance();
                Ok(Expression::Literal(Literal::Number(n)))
            },
            TokenKind::StringLit(s) => {
                self.advance();
                Ok(Expression::Literal(Literal::String(s)))
            },
            TokenKind::True => {
                self.advance();
                Ok(Expression::Literal(Literal::Bool(true)))
            },
            TokenKind::False => {
                self.advance();
                Ok(Expression::Literal(Literal::Bool(false)))
            },
            TokenKind::Identifier(id) => {
                self.advance();
                Ok(Expression::Identifier(id))
            },
            got => Err(format!("Expected literal or identifier, got {:?}", got)),
        }
    }

    // <argument_list> ::= <expression> { "," <expression> }
    fn parse_argument_list(&mut self) -> Result<Vec<Expression>, String> {
        let mut args = Vec::new();
        args.push(self.parse_expression()?);
        while self.peek() == TokenKind::Comma {
            self.advance();
            args.push(self.parse_expression()?);
        }
        Ok(args)
    }
}