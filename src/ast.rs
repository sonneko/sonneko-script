
// ==========================================
// 1. AST (Abstract Syntax Tree) Definitions
// ==========================================

#[derive(Debug, Clone, PartialEq)]
pub enum TypeLiteral {
    Int,
    Float,
    String,
    Bool,
    Custom(String),
}

#[derive(Debug, Clone, PartialEq)]
pub struct Param {
    pub name: String,
    pub type_lit: TypeLiteral,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Decl {
    Function {
        name: String,
        params: Vec<Param>,
        body: Block,
    },
    Object {
        name: String,
        literals: Vec<Param>, // obj_literal uses param syntax inside {}
    },
}

#[derive(Debug, Clone, PartialEq)]
pub struct Block {
    pub statements: Vec<Statement>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Statement {
    Return(Expression),
    Let {
        name: String,
        init: Option<Expression>,
    },
    Expression(Expression),
    If {
        condition: Expression,
        then_block: Block,
        else_ifs: Vec<(Expression, Block)>,
        else_block: Option<Block>,
    },
    While {
        condition: Expression,
        block: Block,
    },
    Break,
    Continue,
}

#[derive(Debug, Clone, PartialEq)]
pub enum BinaryOp {
    Or,          // ||
    And,         // &&
    Eq, Ne,      // ==, !=
    Lt, Le, Gt, Ge, // <, <=, >, >=
    Add, Sub,    // +, -
    Mul, Div,    // *, /
}

#[derive(Debug, Clone, PartialEq)]
pub enum UnaryOp {
    Neg, // -
    Not, // !
}

#[derive(Debug, Clone, PartialEq)]
pub enum Expression {
    Binary {
        left: Box<Expression>,
        op: BinaryOp,
        right: Box<Expression>,
    },
    Unary {
        op: UnaryOp,
        expr: Box<Expression>,
    },
    Call {
        callee: Box<Expression>,
        args: Vec<Expression>,
    },
    Access {
        target: Box<Expression>,
        separator: String, // "." or "::"
        field: String,
    },
    Literal(Literal),
    Identifier(String),
}

#[derive(Debug, Clone, PartialEq)]
pub enum Literal {
    Number(String), // Keep as string to avoid parsing float/int logic here
    String(String),
    Bool(bool),
}

#[derive(Debug, Clone, PartialEq)]
pub struct Program {
    pub decls: Vec<Decl>,
}
