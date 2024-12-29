use std::fmt::Display;

use crate::lexer::{Token, TokenType};

pub struct Parser<'a> {
    tokens: Vec<Token<'a>>,
    current: usize,
}

#[derive(Debug)]
pub enum Expr<'de> {
    Binary {
        left: Box<Expr<'de>>,
        operator: Token<'de>,
        right: Box<Expr<'de>>,
    },
    Unary {
        operator: Token<'de>,
        right: Box<Expr<'de>>,
    },
    Literal(Option<f64>, Option<bool>, Option<&'de str>),
}

impl Display for Expr<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Expr::Literal(Some(num), _, None) => {
                write!(f, "{:?}", num)
            }
            Expr::Literal(None, Some(b), None) => match b {
                true => {
                    write!(f, "true")
                }
                false => {
                    write!(f, "false")
                }
            },
            Expr::Literal(None, None, None) => {
                write!(f, "nil")
            }
            Expr::Literal(None, None, Some(s)) => write!(f, "{s}"),
            _ => todo!(),
        }
    }
}

impl<'de> Parser<'de> {
    pub fn new(tokens: Vec<Token<'de>>) -> Self {
        Parser { tokens, current: 0 }
    }

    pub fn expression(&mut self) -> Result<Expr<'de>, String> {
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
            match token.token_type {
                TokenType::Number(n) => {
                    self.advance();
                    return Ok(Expr::Literal(Some(n), None, None));
                }
                TokenType::String => {
                    self.advance();
                    return Ok(Expr::Literal(None, None, Some(&origin)));
                }
                TokenType::True => {
                    self.advance();
                    return Ok(Expr::Literal(None, Some(true), None));
                }
                TokenType::False => {
                    self.advance();
                    return Ok(Expr::Literal(None, Some(false), None));
                }
                TokenType::Nil => {
                    self.advance();
                    return Ok(Expr::Literal(None, None, None));
                }
                TokenType::LeftParen => {
                    self.advance();
                    let expr = self.expression()?;
                    self.consume(TokenType::RightParen, "Expect ')' after expression")?;
                    return Ok(expr);
                }
                _ => {}
            }
        }

        Err("Expect expression.".to_string())
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
        &self.tokens[0]
    }

    fn is_at_end(&self) -> bool {
        if let Some(token) = self.peek() {
            matches!(token.token_type, TokenType::Eof)
        } else {
            true
        }
    }

    fn consume(&mut self, token_type: TokenType, message: &str) -> Result<&Token<'de>, String> {
        if self.check(&token_type) {
            Ok(self.advance())
        } else {
            Err(message.to_string())
        }
    }
}
