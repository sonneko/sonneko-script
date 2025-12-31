#[derive(Debug, PartialEq, Clone)]
pub enum Token {
    // キーワード
    Fn,  // fn
    Let, // let
    Else,
    If,

    // 識別子とリテラル
    Identifier(String), // 変数名、関数名
    Number(f64),        // 数 (10, 42など)
    String(String),     // 文字列（"hello"など）
    Bool(bool),         // 真偽値（true, false）

    // 演算子
    Plus,     // +
    Minus,    // -
    Asterisk, // *
    Slash,    // /
    Assign,   // =

    // 区切り記号
    LParen,    // (
    RParen,    // )
    LBrace,    // {
    RBrace,    // }
    Comma,     // ,
    SemiColon, // ;

    // 終端
    EOF,
}

use std::iter::Peekable;
use std::str::Chars;

pub struct Lexer<'a> {
    input: Peekable<Chars<'a>>,
}

impl<'a> Lexer<'a> {
    pub fn new(input: &'a str) -> Self {
        Self {
            input: input.chars().peekable(),
        }
    }

    pub fn tokenize(&mut self) -> Vec<Token> {
        let mut tokens = Vec::new();

        while let Some(&c) = self.input.peek() {
            match c {
                ' ' | '\t' | '\n' | '\r' => {
                    self.input.next();
                } // 空白をスキップ
                '+' => {
                    tokens.push(Token::Plus);
                    self.input.next();
                }
                '-' => {
                    tokens.push(Token::Minus);
                    self.input.next();
                }
                '*' => {
                    tokens.push(Token::Asterisk);
                    self.input.next();
                }
                '/' => {
                    tokens.push(Token::Slash);
                    self.input.next();
                }
                '=' => {
                    tokens.push(Token::Assign);
                    self.input.next();
                }
                '(' => {
                    tokens.push(Token::LParen);
                    self.input.next();
                }
                ')' => {
                    tokens.push(Token::RParen);
                    self.input.next();
                }
                '{' => {
                    tokens.push(Token::LBrace);
                    self.input.next();
                }
                '}' => {
                    tokens.push(Token::RBrace);
                    self.input.next();
                }
                ',' => {
                    tokens.push(Token::Comma);
                    self.input.next();
                }
                ';' => {
                    tokens.push(Token::SemiColon);
                    self.input.next();
                }
                '"' => {
                    let mut string_literal = String::new();
                    self.input.next(); // "を消費
                    while let Some(&c) = self.input.peek() {
                        if c == '"' {
                            self.input.next();
                            break;
                        } else {
                            string_literal.push(c);
                            self.input.next();
                        }
                    }
                    tokens.push(Token::String(string_literal));
                }
                '0'..='9' => tokens.push(self.read_number()),
                'a'..='z' | 'A'..='Z' | '_' => tokens.push(self.read_identifier()),
                _ => {
                    panic!("Unknown character: {}", c);
                }
            }
        }
        tokens.push(Token::EOF);
        tokens
    }

    fn read_number(&mut self) -> Token {
        let mut number_str = String::new();
        while let Some(&c) = self.input.peek() {
            if c.is_ascii_digit() || c == '.' {
                number_str.push(self.input.next().unwrap());
            } else {
                break;
            }
        }
        return Token::Number(number_str.parse().unwrap());
    }

    fn read_identifier(&mut self) -> Token {
        let mut id_str = String::new();
        while let Some(&c) = self.input.peek() {
            if c.is_ascii_alphanumeric() || c == '_' {
                id_str.push(self.input.next().unwrap());
            } else {
                break;
            }
        }
        // キーワードの判定
        match id_str.as_str() {
            "fn" => Token::Fn,
            "let" => Token::Let,
            "true" => Token::Bool(true),
            "false" => Token::Bool(false),
            "if" => Token::If,
            "else" => Token::Else,
            _ => Token::Identifier(id_str),
        }
    }
}
