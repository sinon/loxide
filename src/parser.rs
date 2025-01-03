//! Parser
//!
//! Responsible for transforming a given token stream into an AST
//!
//! Uses a recursive desecent parser. To transform the token stream into
//! `Expr`
use std::{fmt::Display, process::ExitCode};

use crate::lexer::{Lexer, Token, TokenType};

/// `Parser` is responsible for iterating over the token stream from `Lexer`
/// and converting the lexed `Token` into `Expr` which represent an Abstract Syntax Tree (AST)
pub struct Parser<'de> {
    tokens: Vec<Token<'de>>,
    current: usize,
    parse_failed: bool,
    has_lex_error: bool,
}

#[derive(Debug, Clone, PartialEq)]
/// `LiteralAtom` represents the types of literals supported by Lox
pub enum LiteralAtom<'de> {
    /// `String` literal for example `"foo"`
    String(&'de str),
    /// Number literal for example `123.1``
    Number(f64),
    /// Nil literal
    Nil,
    /// Bool literals `false` or `true`
    Bool(bool),
}

#[derive(Debug)]
/// `Expr` represents a unit of an AST
pub enum Expr<'de> {
    /// `Binary` is a binary expression such as `1 * 2`
    Binary {
        /// The left item `Expr` in an expression
        left: Box<Expr<'de>>,
        /// The operator to be applied on the `left` and `right` `Expr`
        operator: Token<'de>,
        /// The right item `Expr` in an expression.
        right: Box<Expr<'de>>,
    },
    /// `Unary` is a unary expression such as `!true`
    Unary {
        /// The operator to be applied on the `right` `Expr`
        operator: Token<'de>,
        /// The expression the unary operator will be applied to
        right: Box<Expr<'de>>,
    },
    /// `Literal` is a value
    Literal(LiteralAtom<'de>),
    /// `Grouping` holds other `Expr` such as `(1 * 2)`
    Grouping(Box<Expr<'de>>),
}

#[derive(Debug)]
// `Stmt` represents the possible statements supported
pub enum Stmt<'de> {
    Print(Expr<'de>),
    ExpressionStatement(Expr<'de>),
}

impl Display for Expr<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Expr::Unary { operator, right } => {
                write!(f, "({} {})", operator.origin, right)
            }
            Expr::Binary {
                left,
                operator,
                right,
            } => {
                write!(f, "({} {} {})", operator.origin, left, right)
            }
            Expr::Grouping(exp) => {
                write!(f, "(group {})", exp)
            }
            Expr::Literal(literal_atom) => match literal_atom {
                LiteralAtom::String(cow) => write!(f, "{}", cow),
                LiteralAtom::Number(num) => write!(f, "{:?}", num),
                LiteralAtom::Nil => write!(f, "nil"),
                LiteralAtom::Bool(b) => write!(f, "{b:?}"),
            },
        }
    }
}

impl<'de> Iterator for Parser<'de> {
    type Item = Result<Stmt<'de>, String>;

    fn next(&mut self) -> Option<Self::Item> {
        if self.is_at_end() || self.parse_failed {
            return None;
        }

        let stmt = self.statement();
        match stmt {
            Ok(s) => Some(Ok(s)),
            Err(err) => {
                eprintln!("{}", err);
                self.parse_failed = true;
                Some(Err(err))
            }
        }
    }
}

pub struct EvalParser<'de>(Parser<'de>);
impl<'de> Iterator for EvalParser<'de> {
    type Item = Result<Expr<'de>, String>;

    fn next(&mut self) -> Option<Self::Item> {
        if self.0.is_at_end() || self.0.parse_failed {
            return None;
        }

        let expr = self.0.expression();
        match expr {
            Ok(e) => Some(Ok(e)),
            Err(err) => {
                eprintln!("{}", err);
                self.0.parse_failed = true;
                Some(Err(err))
            }
        }
    }
}

impl<'de> EvalParser<'de> {
    pub fn new(input: &'de str) -> Self {
        let mut tokens = Vec::<Token>::new();
        let mut has_lex_error = false;
        for token in Lexer::new(input) {
            match token {
                Ok(t) => {
                    tokens.push(t);
                }
                Err(_) => {
                    has_lex_error = true;
                    continue;
                }
            }
        }
        EvalParser(Parser {
            tokens,
            current: 0,
            parse_failed: false,
            has_lex_error,
        })
    }
}

impl<'de> Parser<'de> {
    /// Create new `Parser` from a lexed token stream
    pub fn new(input: &'de str) -> Self {
        let mut tokens = Vec::<Token>::new();
        let mut has_lex_error = false;
        for token in Lexer::new(input) {
            match token {
                Ok(t) => {
                    tokens.push(t);
                }
                Err(_) => {
                    has_lex_error = true;
                    continue;
                }
            }
        }
        Parser {
            tokens,
            current: 0,
            parse_failed: false,
            has_lex_error,
        }
    }
    /// Parse the token stream, returning an `ExitCode` to indicate if the process
    /// encountered an error when running
    pub fn parse(&mut self) -> ExitCode {
        let mut exit_code = 0;
        if self.has_lex_error {
            exit_code = 65;
        }
        for exp in self {
            match exp {
                Ok(ex) => {
                    println!("{ex:?}");
                }
                Err(_) => {
                    exit_code = 65;
                }
            }
        }
        ExitCode::from(exit_code)
    }

    fn statement(&mut self) -> Result<Stmt<'de>, String> {
        if self.match_tokens(&[TokenType::Print]) {
            return self.print_statement();
        }
        return self.expression_statement();
    }

    fn expression_statement(&mut self) -> Result<Stmt<'de>, String> {
        let expr = self.expression()?;
        if let Some(token) = self.peek() {
            self.consume(
                TokenType::Semicolon,
                token.origin,
                token.line,
                "Expect ';' after value.",
            )?;
        }
        Ok(Stmt::ExpressionStatement(expr))
    }

    fn print_statement(&mut self) -> Result<Stmt<'de>, String> {
        let expr = self.expression()?;
        if let Some(token) = self.peek() {
            self.consume(
                TokenType::Semicolon,
                token.origin,
                token.line,
                "Expect ';' after value.",
            )?;
        }
        Ok(Stmt::Print(expr))
    }

    fn expression(&mut self) -> Result<Expr<'de>, String> {
        self.equality()
    }

    fn equality(&mut self) -> Result<Expr<'de>, String> {
        let mut expr = self.comparison()?;

        while self.match_tokens(&[TokenType::BangEqual, TokenType::EqualEqual]) {
            let operator = self.previous().clone();
            let right = self.comparison()?;
            expr = Expr::Binary {
                left: Box::new(expr),
                operator,
                right: Box::new(right),
            };
        }

        Ok(expr)
    }

    fn comparison(&mut self) -> Result<Expr<'de>, String> {
        let mut expr = self.term()?;

        while self.match_tokens(&[
            TokenType::Greater,
            TokenType::GreaterEqual,
            TokenType::Less,
            TokenType::LessEqual,
        ]) {
            let operator = self.previous().clone();
            let right = self.term()?;
            expr = Expr::Binary {
                left: Box::new(expr),
                operator,
                right: Box::new(right),
            };
        }

        Ok(expr)
    }

    fn term(&mut self) -> Result<Expr<'de>, String> {
        let mut expr = self.factor()?;

        while self.match_tokens(&[TokenType::Minus, TokenType::Plus]) {
            let operator = self.previous().clone();
            let right = self.factor()?;
            expr = Expr::Binary {
                left: Box::new(expr),
                operator,
                right: Box::new(right),
            };
        }

        Ok(expr)
    }

    fn factor(&mut self) -> Result<Expr<'de>, String> {
        let mut expr = self.unary()?;

        while self.match_tokens(&[TokenType::Slash, TokenType::Star]) {
            let operator = self.previous().clone();
            let right = self.unary()?;
            expr = Expr::Binary {
                left: Box::new(expr),
                operator,
                right: Box::new(right),
            };
        }

        Ok(expr)
    }

    fn unary(&mut self) -> Result<Expr<'de>, String> {
        if self.match_tokens(&[TokenType::Bang, TokenType::Minus]) {
            let operator = self.previous().clone();
            let right = self.unary()?;
            return Ok(Expr::Unary {
                operator,
                right: Box::new(right),
            });
        }

        self.primary()
    }

    fn primary(&mut self) -> Result<Expr<'de>, String> {
        if let Some(token) = self.peek() {
            let origin = token.origin;
            let line_num = token.line;
            match token.token_type {
                TokenType::Number(n) => {
                    self.advance();
                    Ok(Expr::Literal(LiteralAtom::Number(n)))
                }
                TokenType::String => {
                    self.advance();
                    Ok(Expr::Literal(LiteralAtom::String(origin)))
                }
                TokenType::True => {
                    self.advance();
                    Ok(Expr::Literal(LiteralAtom::Bool(true)))
                }
                TokenType::False => {
                    self.advance();
                    Ok(Expr::Literal(LiteralAtom::Bool(false)))
                }
                TokenType::Nil => {
                    self.advance();
                    Ok(Expr::Literal(LiteralAtom::Nil))
                }
                TokenType::LeftParen => {
                    self.advance();
                    let expr = self.expression()?;
                    self.consume(
                        TokenType::RightParen,
                        origin,
                        line_num,
                        "Expect ')' after expression",
                    )?;
                    Ok(Expr::Grouping(Box::new(expr)))
                }
                _ => {
                    let err_msg = self
                        .error_msg(&token.token_type, origin, line_num, "Expect expression")
                        .to_string();
                    Err(err_msg)
                }
            }
        } else {
            Err("error".to_string())
        }
    }

    // Helper methods
    fn match_tokens(&mut self, types: &[TokenType]) -> bool {
        for t in types {
            if self.check(t) {
                self.advance();
                return true;
            }
        }
        false
    }

    fn check(&self, token_type: &TokenType) -> bool {
        if self.is_at_end() {
            return false;
        }
        std::mem::discriminant(&self.peek().unwrap().token_type)
            == std::mem::discriminant(token_type)
    }

    fn advance(&mut self) -> &Token<'de> {
        if !self.is_at_end() {
            self.current += 1;
        }
        self.previous()
    }

    fn peek(&self) -> Option<&Token<'de>> {
        self.tokens.get(self.current)
    }

    fn previous(&self) -> &Token<'de> {
        self.tokens.get(self.current - 1).unwrap()
    }

    fn is_at_end(&self) -> bool {
        if let Some(token) = self.peek() {
            matches!(token.token_type, TokenType::Eof)
        } else {
            true
        }
    }

    fn consume(
        &mut self,
        token_type: TokenType,
        origin: &str,
        num: usize,
        message: &str,
    ) -> Result<&Token<'de>, String> {
        if self.check(&token_type) {
            Ok(self.advance())
        } else {
            Err(self.error_msg(&token_type, origin, num, message))
        }
    }

    fn error_msg(&self, token_type: &TokenType, origin: &str, num: usize, message: &str) -> String {
        if *token_type == TokenType::Eof {
            format!("[line {}] Error at end: {}.", num, message)
        } else {
            format!("[line {}] Error at '{}': {}.", num, origin, message)
        }
    }
}
