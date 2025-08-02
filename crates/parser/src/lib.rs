//! Parser
//!
//! Responsible for transforming a given token stream into an AST
//!
//! Uses a recursive desecent parser. To transform the token stream into
//! `Expr`
use lexer::{Lexer, Token, TokenType};

/// `Parser` is responsible for iterating over the token stream from `Lexer`
/// and converting the lexed `Token` into `Expr` which represent an Abstract Syntax Tree (AST)
pub struct Parser {
    tokens: Vec<Token>,
    current: usize,
    parse_failed: bool,
}

#[derive(Debug, Clone, PartialEq)]
/// `LiteralAtom` represents the types of literals supported by Lox
pub enum LiteralAtom {
    /// `String` literal for example `"foo"`
    String(String),
    /// Number literal for example `123.1`
    Number(f64),
    /// Nil literal
    Nil,
    /// Bool literals `false` or `true`
    Bool(bool),
}

#[derive(Debug, Clone)]
/// `Expr` represents a unit of an AST
pub enum Expr {
    /// `Binary` is a binary expression such as `1 * 2`
    Binary {
        /// The left item `Expr` in an expression
        left: Box<Expr>,
        /// The operator to be applied on the `left` and `right` `Expr`
        operator: Token,
        /// The right item `Expr` in an expression.
        right: Box<Expr>,
    },
    /// `Unary` is a unary expression such as `!true`
    Unary {
        /// The operator to be applied on the `right` `Expr`
        operator: Token,
        /// The expression the unary operator will be applied to
        right: Box<Expr>,
    },
    /// `Literal` is a value
    Literal(LiteralAtom),
    /// `Grouping` holds other `Expr` such as `(1 * 2)`
    Grouping(Box<Expr>),
    /// `Variable`
    Variable(Token),
    /// `Assign`
    Assign(String, Box<Expr>),
    /// `Logical` - `or` and `and`
    Logical {
        /// The left expression of a Logical expression
        left: Box<Expr>,
        /// The operator of a Logical expression
        operator: Token,
        /// The right expression of a Logical expression
        right: Box<Expr>,
    },
    /// Function `Call`
    Call {
        /// function to be called
        callee: Box<Expr>,
        /// paren token
        paren: Token,
        /// arguments to be passed to function call
        arguments: Vec<Expr>,
    },
}

#[derive(Debug, Clone)]
/// `Stmt` represents the possible statements supported
pub enum Stmt {
    /// A print statement
    Print(Expr),
    /// An expression statement
    ExpressionStatement(Expr),
    /// Var statement
    // var <identifier> = expr;
    Var(String, Option<Expr>),
    /// Block
    Block(Vec<Stmt>),
    /// If statement
    If(Expr, Box<Stmt>, Option<Box<Stmt>>),
    /// While statement
    While {
        /// The condition that must be `true` for the body to be run
        condition: Expr,
        /// The statements that will be executed repreatedly if `condition`
        body: Box<Stmt>,
    },
    /// Func statement
    Function {
        name: Token,
        parameters: Vec<Token>,
        body: Vec<Stmt>,
    },
}

impl Iterator for Parser {
    type Item = Result<Stmt, String>;

    fn next(&mut self) -> Option<Self::Item> {
        if self.is_at_end() || self.parse_failed {
            return None;
        }

        let stmt = self.declaration();
        match stmt {
            Ok(s) => Some(Ok(s)),
            Err(err) => {
                eprintln!("{err}");
                self.parse_failed = true;
                Some(Err(err))
            }
        }
    }
}

impl Parser {
    /// Create new `Parser` from a lexed token stream
    #[must_use]
    pub fn new(input: &str) -> Self {
        let tokens: Vec<Token> = Lexer::new(input).flatten().collect();
        Self {
            tokens,
            current: 0,
            parse_failed: false,
        }
    }

    fn declaration(&mut self) -> Result<Stmt, String> {
        if self.match_tokens(&[TokenType::Fun]) {
            return self.function("function");
        }
        if self.match_tokens(&[TokenType::Var]) {
            return self.var_declaration();
        }
        self.statement()
    }

    fn function(&mut self, kind: &str) -> Result<Stmt, String> {
        let name = self
            .consume(&TokenType::Identifier, &format!("Expect {kind} name."))?
            .clone();
        self.consume(
            &TokenType::LeftParen,
            &format!("Expect '(' after {kind} name."),
        )?;
        let mut parameters = Vec::new();
        if !self.check(&TokenType::RightParen) {
            loop {
                if parameters.len() > 255 {
                    return Err("Can't have more than 255 parameters.".to_string());
                }
                // TODO: Figure out right way to avoid this clone
                let param = self
                    .consume(&TokenType::Identifier, "Expect parameter name")?
                    .clone();
                parameters.push(param);
                if !self.match_tokens(&[TokenType::Comma]) {
                    break;
                }
            }
        }
        self.consume(&TokenType::RightParen, "Expect ')' after parameters")?;
        self.consume(&TokenType::LeftBrace, "Expect '{' after parameters")?;
        let body = self.block()?;
        Ok(Stmt::Function {
            name,
            parameters,
            body,
        })
    }

    fn var_declaration(&mut self) -> Result<Stmt, String> {
        let token = self.consume(&TokenType::Identifier, "Expect variable name.")?;
        let name = token.origin.clone();
        let intializer = if self.match_tokens(&[TokenType::Equal]) {
            Some(self.expression()?)
        } else {
            None
        };
        self.consume(&TokenType::Semicolon, "Expect ';' after value.")?;
        Ok(Stmt::Var(name, intializer))
    }

    fn statement(&mut self) -> Result<Stmt, String> {
        if self.match_tokens(&[TokenType::While]) {
            return self.while_statement();
        }
        if self.match_tokens(&[TokenType::For]) {
            return self.for_statement();
        }
        if self.match_tokens(&[TokenType::If]) {
            return self.if_statement();
        }
        if self.match_tokens(&[TokenType::Print]) {
            return self.print_statement();
        }
        if self.match_tokens(&[TokenType::LeftBrace]) {
            let stmts = self.block()?;
            return Ok(Stmt::Block(stmts));
        }
        self.expression_statement()
    }

    fn for_statement(&mut self) -> Result<Stmt, String> {
        self.consume(&TokenType::LeftParen, "Expect '(' after 'for'")?;

        let mut initializer: Option<Stmt> = None;
        if self.match_tokens(&[TokenType::Semicolon]) {
        } else if self.match_tokens(&[TokenType::Var]) {
            initializer = Some(self.var_declaration()?);
        } else {
            initializer = Some(self.expression_statement()?);
        }

        let condition = if self.check(&TokenType::Semicolon) {
            Expr::Literal(LiteralAtom::Bool(true))
        } else {
            self.expression()?
        };
        self.consume(&TokenType::Semicolon, "Expect ';' after loop condition.")?;

        let increment = if self.check(&TokenType::RightParen) {
            None
        } else {
            Some(self.expression()?)
        };
        self.consume(&TokenType::RightParen, "Expect ')' after for clauses.")?;

        let mut body = self.statement()?;

        if let Some(inc) = increment {
            body = Stmt::Block(vec![body, Stmt::ExpressionStatement(inc)]);
        }
        body = Stmt::While {
            condition,
            body: Box::new(body),
        };
        if let Some(init) = initializer {
            body = Stmt::Block(vec![init, body]);
        }
        Ok(body)
    }

    fn while_statement(&mut self) -> Result<Stmt, String> {
        self.consume(&TokenType::LeftParen, "Expect '(' after 'while'")?;
        let condition = self.expression()?;
        self.consume(&TokenType::RightParen, "Expect ')' after condition")?;
        let body = Box::new(self.statement()?);

        Ok(Stmt::While { condition, body })
    }

    fn if_statement(&mut self) -> Result<Stmt, String> {
        self.consume(&TokenType::LeftParen, "Expect ')' after 'if'")?;
        let condition = self.expression()?;
        self.consume(&TokenType::RightParen, "Expect ')' after if condition.")?;

        let then_branch = self.statement()?;
        let else_branch = if self.match_tokens(&[TokenType::Else]) {
            Some(Box::new(self.statement()?))
        } else {
            None
        };
        let if_stmt = Stmt::If(condition, Box::new(then_branch), else_branch);
        Ok(if_stmt)
    }

    fn block(&mut self) -> Result<Vec<Stmt>, String> {
        let mut stmts: Vec<Stmt> = Vec::new();
        while !self.check(&TokenType::RightBrace) && !self.is_at_end() {
            stmts.push(self.declaration()?);
        }
        self.consume(&TokenType::RightBrace, "Expect '}' after block")?;
        Ok(stmts)
    }

    fn expression_statement(&mut self) -> Result<Stmt, String> {
        let expr = self.expression()?;
        self.consume(&TokenType::Semicolon, "Expect ';' after value")?;
        Ok(Stmt::ExpressionStatement(expr))
    }

    fn print_statement(&mut self) -> Result<Stmt, String> {
        let expr = self.expression()?;
        self.consume(&TokenType::Semicolon, "Expect ';' after value")?;
        Ok(Stmt::Print(expr))
    }

    fn expression(&mut self) -> Result<Expr, String> {
        self.assignment()
    }

    fn assignment(&mut self) -> Result<Expr, String> {
        let expr = self.or()?;

        if self.match_tokens(&[TokenType::Equal]) {
            let equals = self.previous().clone();
            let value = self.assignment()?;

            if let Expr::Variable(token) = expr {
                return Ok(Expr::Assign(token.origin, Box::new(value)));
            }
            let err_msg = Self::error_msg(
                &equals.token_type,
                &equals.origin,
                equals.line,
                "Invalid assignment type.",
            );
            return Err(err_msg);
        }

        Ok(expr)
    }

    fn or(&mut self) -> Result<Expr, String> {
        let mut expr = self.and()?;

        while self.match_tokens(&[TokenType::Or]) {
            let operator = self.previous().clone();
            let right = self.and()?;
            expr = Expr::Logical {
                left: Box::new(expr),
                operator,
                right: Box::new(right),
            };
        }

        Ok(expr)
    }

    fn and(&mut self) -> Result<Expr, String> {
        let mut expr = self.equality()?;
        while self.match_tokens(&[TokenType::And]) {
            let operator = self.previous().clone();
            let right = self.equality()?;
            expr = Expr::Logical {
                left: Box::new(expr),
                operator,
                right: Box::new(right),
            }
        }
        Ok(expr)
    }

    fn equality(&mut self) -> Result<Expr, String> {
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

    fn comparison(&mut self) -> Result<Expr, String> {
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

    fn term(&mut self) -> Result<Expr, String> {
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

    fn factor(&mut self) -> Result<Expr, String> {
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

    fn unary(&mut self) -> Result<Expr, String> {
        if self.match_tokens(&[TokenType::Bang, TokenType::Minus]) {
            let operator = self.previous().clone();
            let right = self.unary()?;
            return Ok(Expr::Unary {
                operator,
                right: Box::new(right),
            });
        }

        self.call()
    }

    fn call(&mut self) -> Result<Expr, String> {
        let mut expr = self.primary()?;
        loop {
            if self.match_tokens(&[TokenType::LeftParen]) {
                expr = self.finish_call(expr)?;
            } else {
                break;
            }
        }
        Ok(expr)
    }

    fn finish_call(&mut self, callee: Expr) -> Result<Expr, String> {
        let mut arguments: Vec<Expr> = vec![];

        if !self.check(&TokenType::RightParen) {
            loop {
                arguments.push(self.expression()?);
                // https://users.rust-lang.org/t/how-many-arguments-can-i-pass-to-a-function/84250
                if arguments.len() >= 65535 {
                    return Err("Can't have more than 65535 arguments.".to_string());
                }
                if !self.match_tokens(&[TokenType::Comma]) {
                    break;
                }
            }
        }
        let paren = self
            .consume(&TokenType::RightParen, "Expect ')' after arguments.")?
            .clone();
        Ok(Expr::Call {
            callee: Box::new(callee),
            paren,
            arguments,
        })
    }

    fn primary(&mut self) -> Result<Expr, String> {
        let token = self.advance();
        match token.token_type {
            TokenType::Number(n) => Ok(Expr::Literal(LiteralAtom::Number(n))),
            TokenType::String => Ok(Expr::Literal(LiteralAtom::String(token.origin.clone()))),
            TokenType::True => Ok(Expr::Literal(LiteralAtom::Bool(true))),
            TokenType::False => Ok(Expr::Literal(LiteralAtom::Bool(false))),
            TokenType::Nil => Ok(Expr::Literal(LiteralAtom::Nil)),
            TokenType::LeftParen => {
                let expr = self.expression()?;
                self.consume(&TokenType::RightParen, "Expect ')' after expression")?;
                Ok(Expr::Grouping(Box::new(expr)))
            }
            TokenType::Identifier => Ok(Expr::Variable(self.previous().clone())),
            _ => {
                let err_msg = Self::error_msg(
                    &token.token_type,
                    &token.origin,
                    token.line,
                    "Expect expression",
                );
                Err(err_msg)
            }
        }
    }

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

    fn advance(&mut self) -> &Token {
        if !self.is_at_end() {
            self.current += 1;
        }
        self.previous()
    }

    fn peek(&self) -> &Token {
        self.tokens
            .get(self.current)
            .expect("current should never be outside index")
    }

    fn previous(&self) -> &Token {
        self.tokens.get(self.current - 1).unwrap()
    }

    fn is_at_end(&self) -> bool {
        let token = self.peek();
        matches!(token.token_type, TokenType::Eof)
    }

    fn consume(&mut self, token_type: &TokenType, message: &str) -> Result<&Token, String> {
        if self.check(token_type) {
            Ok(self.advance())
        } else {
            let token = self.peek();
            Err(Self::error_msg(
                token_type,
                &token.origin,
                token.line,
                message,
            ))
        }
    }

    fn error_msg(token_type: &TokenType, origin: &str, num: usize, message: &str) -> String {
        if *token_type == TokenType::Eof {
            format!("[line {num}] Error at end: {message}.")
        } else {
            format!("[line {num}] Error at '{origin}': {message}.")
        }
    }
}
