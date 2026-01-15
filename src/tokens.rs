
// ==========================================
// 2. Token Definitions
// ==========================================

#[derive(Debug, Clone, PartialEq)]
pub enum TokenKind {
    // Keywords
    Fn, Let, If, Else, While, Break, Continue, Return, Obj,
    True, False,
    
    // Type Literals (Keywords)
    TypeInt, TypeFloat, TypeString, TypeBool,

    // Symbols
    LBrace, RBrace,     // { }
    LParen, RParen,     // ( )
    Comma,              // ,
    Colon,              // :
    SemiColon,          // ;
    Assign,             // =
    
    // Operators
    Or, And,            // ||, &&
    Eq, Ne,             // ==, !=
    Lt, Le, Gt, Ge,     // <, <=, >, >=
    Plus, Minus,        // +, -
    Star, Slash,        // *, /
    Not,                // !
    Dot, DoubleColon,   // ., ::

    // Literals & Identifiers
    Identifier(String),
    NumberLit(String),
    StringLit(String),

    EOF,
}

#[derive(Debug, Clone)]
pub struct Token {
    pub kind: TokenKind,
    // In a real parser, we would store span/location here
}
