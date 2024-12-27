use std::fmt;

use miette::{miette, Error, LabeledSpan, Result};
pub struct Token<'de> {
    token_type: TokenType,
    origin: &'de str,
}

#[derive(PartialEq)]
pub enum TokenType {
    // single-character
    RightParen,
    LeftParen,
    RightBrace,
    LeftBrace,
    Comma,
    Dot,
    Minus,
    Plus,
    Semicolon,
    Slash,
    Star,
    // one or two character tokens
    Bang,
    BangEqual,
    Equal,
    EqualEqual,
    Greater,
    GreaterEqual,
    Less,
    LessEqual,
    // literals
    String,
    Identifier,
    Number(f64),
    // Keywords.
    And,
    Class,
    Else,
    False,
    Fun,
    For,
    If,
    Nil,
    Or,
    Print,
    Return,
    Super,
    This,
    True,
    Var,
    While,
    // Misc
    Eof,
}

impl<'de> fmt::Display for Token<'de> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let i = self.origin;
        match self.token_type {
            TokenType::RightParen => write!(f, "RIGHT_PAREN {i} null"),
            TokenType::LeftParen => write!(f, "LEFT_PAREN {i} null"),
            TokenType::Eof => write!(f, "EOF  null"),
            TokenType::RightBrace => write!(f, "RIGHT_BRACE {i} null"),
            TokenType::LeftBrace => write!(f, "LEFT_BRACE {i} null"),
            TokenType::Comma => write!(f, "COMMA {i} null"),
            TokenType::Dot => write!(f, "DOT {i} null"),
            TokenType::Minus => write!(f, "MINUS {i} null"),
            TokenType::Plus => write!(f, "PLUS {i} null"),
            TokenType::Star => write!(f, "STAR {i} null"),
            TokenType::Semicolon => write!(f, "SEMICOLON {i} null"),
            TokenType::Equal => write!(f, "EQUAL {i} null"),
            TokenType::EqualEqual => write!(f, "EQUAL_EQUAL {i} null"),
            TokenType::Bang => write!(f, "BANG {i} null"),
            TokenType::BangEqual => write!(f, "BANG_EQUAL {i} null"),
            TokenType::Less => write!(f, "LESS {i} null"),
            TokenType::LessEqual => write!(f, "LESS_EQUAL {i} null"),
            TokenType::Greater => write!(f, "GREATER {i} null"),
            TokenType::GreaterEqual => write!(f, "GREATER_EQUAL {i} null"),
            TokenType::Slash => write!(f, "SLASH {i} null"),
            TokenType::String => write!(f, "STRING \"{i}\" {i}"),
            TokenType::Identifier => write!(f, "IDENTIFIER {i} null"),
            TokenType::Number(n) => write!(f, "NUMBER {i} {n:?}"),
            TokenType::And => todo!(),
            TokenType::Class => todo!(),
            TokenType::Else => todo!(),
            TokenType::False => todo!(),
            TokenType::Fun => todo!(),
            TokenType::For => todo!(),
            TokenType::If => todo!(),
            TokenType::Nil => todo!(),
            TokenType::Or => todo!(),
            TokenType::Print => todo!(),
            TokenType::Return => todo!(),
            TokenType::Super => todo!(),
            TokenType::This => todo!(),
            TokenType::True => todo!(),
            TokenType::Var => todo!(),
            TokenType::While => todo!(),
        }
    }
}

pub struct Lexer<'de> {
    whole: &'de str,
    rest: &'de str,
    byte: usize,
    line_num: usize,
}

impl<'de> Lexer<'de> {
    pub fn new(input: &'de str) -> Self {
        Self {
            rest: input,
            whole: input,
            byte: 0,
            line_num: 1,
        }
    }
}

impl<'de> Iterator for Lexer<'de> {
    type Item = Result<Token<'de>, Error>;

    fn next(&mut self) -> Option<Self::Item> {
        loop {
            let mut chars = self.rest.char_indices();
            let (at, c) = chars.next()?;
            let mut c_str = &self.rest[at..at + c.len_utf8()];
            let c_onwards = self.rest;
            self.rest = chars.as_str();
            self.byte += c.len_utf8();

            enum Started {
                String,
                Number,
                Identifier,
                IfNextEqual(TokenType, TokenType),
                CodeComment,
            }

            let res = move |t_type: TokenType| {
                Some(Ok(Token {
                    token_type: t_type,
                    origin: c_str,
                }))
            };

            let started = match c {
                '(' => return res(TokenType::LeftParen),
                ')' => return res(TokenType::RightParen),
                '{' => return res(TokenType::LeftBrace),
                '}' => return res(TokenType::RightBrace),
                ',' => return res(TokenType::Comma),
                '.' => return res(TokenType::Dot),
                '-' => return res(TokenType::Minus),
                '+' => return res(TokenType::Plus),
                '*' => return res(TokenType::Star),
                ';' => return res(TokenType::Semicolon),
                '"' => Started::String,
                '0'..='9' => Started::Number,
                'a'..='z' => Started::Identifier,
                '_' => Started::Identifier,
                '<' => Started::IfNextEqual(TokenType::LessEqual, TokenType::Less),
                '>' => Started::IfNextEqual(TokenType::GreaterEqual, TokenType::Greater),
                '!' => Started::IfNextEqual(TokenType::BangEqual, TokenType::Bang),
                '=' => Started::IfNextEqual(TokenType::EqualEqual, TokenType::Equal),
                '/' => Started::CodeComment,
                c if c.is_whitespace() => {
                    if c == '\n' {
                        self.line_num += 1;
                    }
                    continue;
                },
                _ => {
                        return Some(Err(
                            miette! {labels = vec![LabeledSpan::at(self.byte-c.len_utf8()..self.byte, "this character")], 
                            "[line {}] Error: Unexpected character: {c}", self.line_num}
                            .with_source_code(self.whole.to_string()),
                        ))
                    }
            };

            match started {
                Started::CodeComment => {
                    if self.rest.starts_with("/") {
                        let eol = self.rest.find("\n");
                        match eol {
                            Some(idx) => {
                                self.byte += idx;
                                self.rest = &self.rest[idx..];
                            }
                            None => {
                                self.byte = self.whole.len();
                                self.rest = &self.rest[self.rest.len()..self.rest.len()];
                                return None;
                            }
                        }
                    } else {
                        return Some(Ok(Token {
                            token_type: TokenType::Slash,
                            origin: c_str,
                        }));
                    }
                }
                Started::String => {
                    if !self.rest.contains('"') {
                        // Scan to end, we cannot continue to scan for tokens when in an unterminated string
                        self.byte = self.whole.len();
                        self.rest = &self.rest[self.rest.len()..self.rest.len()];
                        return Some(Err(
                        miette! {labels = vec![LabeledSpan::at(self.byte -c.len_utf8()..self.byte, "this unterminated string")],
                        "[line {}] Error: Unterminated string.", self.line_num}.with_source_code(self.whole.to_string())
                    ));
                    } else {
                        let (s, _) = self.rest.split_once("\"")?;
                        let c_str = &c_onwards[1..s.len() + 1];
                        self.byte += s.len() + 1;
                        self.rest = &self.rest[s.len() + 1..];
                        return Some(Ok(Token {
                            token_type: TokenType::String,
                            origin: c_str,
                        }));
                    }
                }
                Started::Number => loop {
                    let next_num = chars.next();

                    match next_num {
                        Some((_, cn)) => {
                            if cn.is_numeric() || cn == '.' {
                                c_str = &c_onwards[at..c_str.len() + cn.len_utf8()];
                                self.rest = chars.as_str();
                                self.byte += cn.len_utf8();
                                continue;
                            } else {
                                let num: f64 = c_str.parse().unwrap();
                                return Some(Ok(Token {
                                    token_type: TokenType::Number(num),
                                    origin: c_str,
                                }));
                            }
                        }
                        None => {
                            let num: f64 = c_str.parse().unwrap();
                            return Some(Ok(Token {
                                token_type: TokenType::Number(num),
                                origin: c_str,
                            }));
                        }
                    }
                },
                Started::Identifier => loop {
                    let next_char = chars.next();

                    match next_char {
                        Some((_, cn)) => {
                            if cn.is_alphanumeric() || cn == '_' {
                                c_str = &c_onwards[at..c_str.len() + cn.len_utf8()];
                                self.rest = chars.as_str();
                                self.byte += cn.len_utf8();
                                continue;
                            } else {
                                return Some(Ok(Token {
                                    token_type: TokenType::Identifier,
                                    origin: c_str,
                                }));
                            }
                        }
                        None => {
                            return Some(Ok(Token {
                                token_type: TokenType::Identifier,
                                origin: c_str,
                            }));
                        }
                    }
                },
                Started::IfNextEqual(then, else_t) => {
                    if self.rest.starts_with("=") {
                        let c_str = &c_onwards[..2];
                        self.byte += 1;
                        self.rest = &self.rest[1..];
                        let token = Token {
                            token_type: then,
                            origin: c_str,
                        };
                        return Some(Ok(token));
                    } else {
                        let token = Token {
                            token_type: else_t,
                            origin: c_str,
                        };
                        return Some(Ok(token));
                    }
                }
            };
        }
    }
}
