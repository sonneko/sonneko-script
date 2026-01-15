use regex::Regex;
use crate::tokens::{Token, TokenKind};

// ==========================================
// 3. Tokenizer (Lexer)
// ==========================================

struct TokenRule {
    regex: Regex,
    generator: Box<dyn Fn(String) -> TokenKind>,
}

pub struct Lexer {
    rules: Vec<TokenRule>,
}

impl Lexer {
    pub fn new() -> Self {
        let mut rules = Vec::new();

        macro_rules! rule {
            ($pat:expr, $conv:expr) => {
                rules.push(TokenRule {
                    regex: Regex::new(&format!("^{}", $pat)).unwrap(),
                    generator: Box::new($conv),
                });
            };
        }

        // Whitespace is handled in the tokenization loop by skipping

        // Keywords and Types
        // Note: Check keywords before identifiers
        rule!(r"fn\b", |_| TokenKind::Fn);
        rule!(r"let\b", |_| TokenKind::Let);
        rule!(r"if\b", |_| TokenKind::If);
        rule!(r"else\b", |_| TokenKind::Else);
        rule!(r"while\b", |_| TokenKind::While);
        rule!(r"break\b", |_| TokenKind::Break);
        rule!(r"continue\b", |_| TokenKind::Continue);
        rule!(r"return\b", |_| TokenKind::Return);
        rule!(r"obj\b", |_| TokenKind::Obj);
        rule!(r"true\b", |_| TokenKind::True);
        rule!(r"false\b", |_| TokenKind::False);
        
        rule!(r"Int\b", |_| TokenKind::TypeInt);
        rule!(r"Float\b", |_| TokenKind::TypeFloat);
        rule!(r"String\b", |_| TokenKind::TypeString);
        rule!(r"Bool\b", |_| TokenKind::TypeBool);

        // Multi-char Operators
        rule!(r"\|\|", |_| TokenKind::Or);
        rule!(r"&&", |_| TokenKind::And);
        rule!(r"==", |_| TokenKind::Eq);
        rule!(r"!=", |_| TokenKind::Ne);
        rule!(r"<=", |_| TokenKind::Le);
        rule!(r">=", |_| TokenKind::Ge);
        rule!(r"::", |_| TokenKind::DoubleColon);

        // Single-char Symbols
        rule!(r"\{", |_| TokenKind::LBrace);
        rule!(r"\}", |_| TokenKind::RBrace);
        rule!(r"\(", |_| TokenKind::LParen);
        rule!(r"\)", |_| TokenKind::RParen);
        rule!(r",", |_| TokenKind::Comma);
        rule!(r":", |_| TokenKind::Colon);
        rule!(r";", |_| TokenKind::SemiColon);
        rule!(r"=", |_| TokenKind::Assign);
        rule!(r"<", |_| TokenKind::Lt);
        rule!(r">", |_| TokenKind::Gt);
        rule!(r"\+", |_| TokenKind::Plus);
        rule!(r"-", |_| TokenKind::Minus);
        rule!(r"\*", |_| TokenKind::Star);
        rule!(r"/", |_| TokenKind::Slash);
        rule!(r"!", |_| TokenKind::Not);
        rule!(r"\.", |_| TokenKind::Dot);

        // Literals
        // Number: simple integer or float regex
        rule!(r"[0-9]+(\.[0-9]+)?", |s| TokenKind::NumberLit(s));
        
        // String: starts with " and ends with " (simplistic, no escape handling for brevity)
        rule!(r#""[^"]*""#, |s| {
            // Remove quotes
            TokenKind::StringLit(s[1..s.len()-1].to_string())
        });

        // Identifier
        rule!(r"[a-zA-Z_@][a-zA-Z0-9_]*", |s| TokenKind::Identifier(s));

        Self { rules }
    }

    pub fn tokenize(&self, mut input: &str) -> Result<Vec<Token>, String> {
        let mut tokens = Vec::new();

        while !input.is_empty() {
            // Skip whitespace
            if let Some(rest) = input.strip_prefix(|c: char| c.is_whitespace()) {
                input = rest;
                // Continue trimming until no whitespace
                input = input.trim_start(); 
                if input.is_empty() { break; }
            }

            let mut matched = false;
            for rule in &self.rules {
                if let Some(mat) = rule.regex.find(input) {
                    let text = mat.as_str().to_string();
                    let kind = (rule.generator)(text);
                    tokens.push(Token { kind });
                    input = &input[mat.end()..];
                    matched = true;
                    break;
                }
            }

            if !matched {
                return Err(format!("Unexpected character at: {}", input));
            }
        }

        tokens.push(Token { kind: TokenKind::EOF });
        Ok(tokens)
    }
}
