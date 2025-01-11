//! Parser
//!
//! Responsible for transforming a given token stream into an AST
//!
//! Uses a recursive desecent parser. To transform the token stream into
//! `Expr`
use crate::lexer::{Lexer, Token, TokenType};

/// `Parser` is responsible for iterating over the token stream from `Lexer`
/// and converting the lexed `Token` into `Expr` which represent an Abstract Syntax Tree (AST)
pub struct Parser<'de> {
    tokens: Vec<Token<'de>>,
    current: usize,
    parse_failed: bool,
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
    /// `Variable`
    Variable(Token<'de>),
    /// `Assign`
    Assign(&'de str, Box<Expr<'de>>),
}

#[derive(Debug)]
/// `Stmt` represents the possible statements supported
pub enum Stmt<'de> {
    /// A print statement
    Print(Expr<'de>),
    /// An expression statement
    ExpressionStatement(Expr<'de>),
    /// Var statement
    Var(&'de str, Option<Expr<'de>>),
    /// Block
    Block(Vec<Stmt<'de>>),
}

impl<'de> Iterator for Parser<'de> {
    type Item = Result<Stmt<'de>, String>;

    fn next(&mut self) -> Option<Self::Item> {
        if self.is_at_end() || self.parse_failed {
            return None;
        }

        let stmt = self.declaration();
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

impl<'de> Parser<'de> {
    /// Create new `Parser` from a lexed token stream
    pub fn new(input: &'de str) -> Self {
        let mut tokens = Vec::<Token>::new();
        for token in Lexer::new(input) {
            match token {
                Ok(t) => {
                    tokens.push(t);
                }
                Err(_) => {
                    continue;
                }
            }
        }
        Parser {
            tokens,
            current: 0,
            parse_failed: false,
        }
    }

    fn declaration(&mut self) -> Result<Stmt<'de>, String> {
        if self.match_tokens(&[TokenType::Var]) {
            return self.var_declaration();
        }
        self.statement()
    }

    fn var_declaration(&mut self) -> Result<Stmt<'de>, String> {
        let token = self.peek();
        let name = self
            .consume(
                TokenType::Identifier,
                token.origin,
                token.line,
                "Expect variable name.",
            )?
            .origin;
        let mut intializer = None;
        if self.match_tokens(&[TokenType::Equal]) {
            intializer = Some(self.expression()?);
        }
        let token = self.peek();
        self.consume(
            TokenType::Semicolon,
            token.origin,
            token.line,
            "Expect ';' after value.",
        )?;
        Ok(Stmt::Var(name, intializer))
    }

    fn statement(&mut self) -> Result<Stmt<'de>, String> {
        if self.match_tokens(&[TokenType::Print]) {
            return self.print_statement();
        }
        if self.match_tokens(&[TokenType::LeftBrace]) {
            let stmts = self.block()?;
            return Ok(Stmt::Block(stmts));
        }
        self.expression_statement()
    }

    fn block(&mut self) -> Result<Vec<Stmt<'de>>, String> {
        let mut stmts: Vec<Stmt<'de>> = Vec::new();
        while !self.check(&TokenType::RightBrace) && !self.is_at_end() {
            stmts.push(self.declaration()?)
        }

        let token = self.peek();
        self.consume(
            TokenType::RightBrace,
            token.origin,
            token.line,
            "Expect '}' after block.",
        )?;
        Ok(stmts)
    }

    fn expression_statement(&mut self) -> Result<Stmt<'de>, String> {
        let expr = self.expression()?;
        let token = self.peek();
        self.consume(
            TokenType::Semicolon,
            token.origin,
            token.line,
            "Expect ';' after value.",
        )?;
        Ok(Stmt::ExpressionStatement(expr))
    }

    fn print_statement(&mut self) -> Result<Stmt<'de>, String> {
        let expr = self.expression()?;
        let token = self.peek();
        self.consume(
            TokenType::Semicolon,
            token.origin,
            token.line,
            "Expect ';' after value.",
        )?;
        Ok(Stmt::Print(expr))
    }

    fn expression(&mut self) -> Result<Expr<'de>, String> {
        self.assignment()
    }

    fn assignment(&mut self) -> Result<Expr<'de>, String> {
        let expr = self.equality()?;

        if self.match_tokens(&[TokenType::Equal]) {
            let equals = self.previous().clone();
            let value = self.assignment()?;

            match expr {
                Expr::Variable(token) => {
                    let name = token.origin;
                    return Ok(Expr::Assign(name, Box::new(value)));
                }
                _ => {
                    let err_msg = self.error_msg(
                        &equals.token_type,
                        equals.origin,
                        equals.line,
                        "Invalid assignment type.",
                    );
                    return Err(err_msg.to_string());
                }
            }
        }

        Ok(expr)
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
        let token = self.peek();
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
            TokenType::Identifier => {
                self.advance();
                Ok(Expr::Variable(self.previous().clone()))
            }
            _ => {
                let err_msg = self
                    .error_msg(&token.token_type, origin, line_num, "Expect expression")
                    .to_string();
                Err(err_msg)
            }
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
        std::mem::discriminant(&self.peek().token_type) == std::mem::discriminant(token_type)
    }

    fn advance(&mut self) -> &Token<'de> {
        if !self.is_at_end() {
            self.current += 1;
        }
        self.previous()
    }

    fn peek(&self) -> &Token<'de> {
        self.tokens
            .get(self.current)
            .expect("current should never be outside index")
    }

    fn previous(&self) -> &Token<'de> {
        self.tokens.get(self.current - 1).unwrap()
    }

    fn is_at_end(&self) -> bool {
        let token = self.peek();
        matches!(token.token_type, TokenType::Eof)
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
