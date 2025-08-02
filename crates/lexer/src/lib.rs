//! Lexer
//!
//! Responsible for transforming a given input str into a Iterator of `Result<Token>`
#![allow(clippy::too_many_lines)]

use miette::{Error, LabeledSpan, Result, miette};

#[derive(Clone, Debug)]
pub struct Span {
    pub start: usize,
    pub end: usize,
}

/// `Token` is formed of a token type (`TokenType`) and a reference to a str in the input string
#[derive(Clone, Debug)]
pub struct Token {
    /// The `TokenType` of `Token`
    pub token_type: TokenType,
    /// The text reference from the source code
    pub span: Span,
    /// The text reference from the source code
    pub origin: String,
    /// The line number where the token was parsed from
    pub line: usize,
}

/// `TokenType` are the valid tokens types that lox code can be lexed into.
#[derive(PartialEq, Debug, Clone, Copy)]
#[doc(hidden)]
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

/// `Lexer` is responsible for iterating over a input string and emitting `Token` for
/// each detected `TokenType`. It maintains the following state:
///
/// `rest` - which is a reference to the substring of `whole` which remained to be lexed.
/// `byte` - the current offset of the byte within the input string being lexed.
/// `line_num` - the current line being processed by the lexer. 1-indexed, used for error reporting.
pub struct Lexer<'de> {
    whole: &'de str,
    rest: &'de str,
    byte: usize,
    line_num: usize,
    at_eof: bool,
}

impl<'de> Lexer<'de> {
    /// Create a new instance of `Lexer` for the given input str.
    #[must_use]
    pub const fn new(input: &'de str) -> Self {
        Self {
            rest: input,
            whole: input,
            byte: 0,
            line_num: 1,
            at_eof: false,
        }
    }
    fn match_reserved_word(c_str: &str) -> TokenType {
        match c_str {
            "and" => TokenType::And,
            "class" => TokenType::Class,
            "else" => TokenType::Else,
            "false" => TokenType::False,
            "fun" => TokenType::Fun,
            "for" => TokenType::For,
            "if" => TokenType::If,
            "nil" => TokenType::Nil,
            "or" => TokenType::Or,
            "print" => TokenType::Print,
            "return" => TokenType::Return,
            "super" => TokenType::Super,
            "this" => TokenType::This,
            "true" => TokenType::True,
            "var" => TokenType::Var,
            "while" => TokenType::While,
            _ => TokenType::Identifier,
        }
    }
}

enum Started {
    String,
    Number,
    Identifier,
    IfNextEqual(TokenType, TokenType),
    CodeComment,
}

impl Iterator for Lexer<'_> {
    type Item = Result<Token, Error>;

    fn next(&mut self) -> Option<Self::Item> {
        if self.at_eof {
            return None;
        }
        loop {
            let mut chars = self.rest.char_indices();
            if let Some(x) = chars.next() {
                let (at, c) = x;
                let mut c_str = &self.rest[at..at + c.len_utf8()];
                let c_onwards = self.rest;
                self.rest = chars.as_str();
                let start_byte = self.byte;
                self.byte += c.len_utf8();

                let started = match c {
                    '(' => {
                        return Some(Ok(Token {
                            token_type: TokenType::LeftParen,
                            span: Span { start: start_byte, end: start_byte},
                            origin: c_str.to_string(),
                            line: self.line_num,
                        }))
                    }
                    ')' => {
                        return Some(Ok(Token {
                            token_type: TokenType::RightParen,
                            span: Span { start: start_byte, end: start_byte},
                            origin: c_str.to_string(),
                            line: self.line_num,
                        }))
                    }
                    '{' => {
                        return Some(Ok(Token {
                            token_type: TokenType::LeftBrace,
                            span: Span { start: start_byte, end: start_byte},
                            origin: c_str.to_string(),
                            line: self.line_num,
                        }))
                    }
                    '}' => {
                        return Some(Ok(Token {
                            token_type: TokenType::RightBrace,
                            span: Span { start: start_byte, end: start_byte},
                            origin: c_str.to_string(),
                            line: self.line_num,
                        }))
                    }
                    ',' => {
                        return Some(Ok(Token {
                            token_type: TokenType::Comma,
                            span: Span { start: start_byte, end: start_byte},
                            origin: c_str.to_string(),
                            line: self.line_num,
                        }))
                    }
                    '.' => {
                        return Some(Ok(Token {
                            token_type: TokenType::Dot,
                            span: Span { start: start_byte, end: start_byte},
                            origin: c_str.to_string(),
                            line: self.line_num,
                        }))
                    }
                    '-' => {
                        return Some(Ok(Token {
                            token_type: TokenType::Minus,
                            span: Span { start: start_byte, end: start_byte},
                            origin: c_str.to_string(),
                            line: self.line_num,
                        }))
                    }
                    '+' => {
                        return Some(Ok(Token {
                            token_type: TokenType::Plus,
                            span: Span { start: start_byte, end: start_byte},
                            origin: c_str.to_string(),
                            line: self.line_num,
                        }))
                    }
                    '*' => {
                        return Some(Ok(Token {
                            token_type: TokenType::Star,
                            span: Span { start: start_byte, end: start_byte},
                            origin: c_str.to_string(),
                            line: self.line_num,
                        }))
                    }
                    ';' => {
                        return Some(Ok(Token {
                            token_type: TokenType::Semicolon,
                            span: Span { start: start_byte, end: start_byte},
                            origin: c_str.to_string(),
                            line: self.line_num,
                        }))
                    }
                    '"' => Started::String,
                    '0'..='9' => Started::Number,
                    'a'..='z' | 'A'..='Z' | '_' => Started::Identifier,
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
                    }
                    _ => {
                        return Some(Err(miette! {
                            labels = vec![LabeledSpan::at(self.byte-c.len_utf8()..self.byte, "this character")],
                            "[line {}] Error: Unexpected character: {c}", self.line_num
                        }
                        .with_source_code(self.whole.to_string())))
                    }
                };
                match started {
                    Started::CodeComment => {
                        if self.rest.starts_with('/') {
                            let eol = self.rest.find('\n');
                            if let Some(idx) = eol {
                                self.byte += idx;
                                self.rest = &self.rest[idx..];
                            } else {
                                self.byte = self.whole.len();
                                self.rest = &self.rest[self.rest.len()..self.rest.len()];
                            }
                        } else {
                            return Some(Ok(Token {
                                token_type: TokenType::Slash,
                                span: Span {
                                    start: start_byte,
                                    end: self.byte,
                                },
                                origin: c_str.to_string(),
                                line: self.line_num,
                            }));
                        }
                    }
                    Started::String => {
                        if self.rest.contains('"') {
                            let (s, _) = self.rest.split_once('"')?;
                            let c_str = &c_onwards[1..=s.len()];
                            self.byte += s.len() + 1;
                            self.rest = &self.rest[s.len() + 1..];
                            return Some(Ok(Token {
                                token_type: TokenType::String,
                                span: Span {
                                    start: start_byte,
                                    end: self.byte - 1,
                                },
                                origin: c_str.to_string(),
                                line: self.line_num,
                            }));
                        }
                        // Scan to end, we cannot continue to scan for tokens when in an unterminated string
                        self.byte = self.whole.len();
                        self.rest = &self.rest[self.rest.len()..self.rest.len()];
                        return Some(Err(miette! {
                            labels = vec![LabeledSpan::at(start_byte..self.byte, "this unterminated string")],
                            "[line {}] Error: Unterminated string.", self.line_num
                        }
                        .with_source_code(self.whole.to_string())));
                    }
                    Started::Number => loop {
                        let next_num = chars.next();
                        if let Some((_, cn)) = next_num {
                            if cn.is_numeric() {
                                c_str = &c_onwards[at..c_str.len() + cn.len_utf8()];
                                self.rest = chars.as_str();
                                self.byte += cn.len_utf8();
                            } else if cn == '.' {
                                if let Some((_, c_peek)) = chars.next() {
                                    // 456. != 456.0 but unstead 456 DOT
                                    if c_peek.is_numeric() {
                                        c_str = &c_onwards
                                            [at..c_str.len() + cn.len_utf8() + c_peek.len_utf8()];
                                        self.rest = chars.as_str();
                                        self.byte += cn.len_utf8() + c_peek.len_utf8();
                                    } else {
                                        let num = c_str.parse().expect(
                                            "We have called is_numeric on each char in `c_str`",
                                        );
                                        return Some(Ok(Token {
                                            token_type: TokenType::Number(num),
                                            span: Span {
                                                start: start_byte,
                                                end: self.byte - 1,
                                            },
                                            origin: c_str.to_string(),
                                            line: self.line_num,
                                        }));
                                    }
                                }
                            } else {
                                let num: f64 = c_str
                                    .parse()
                                    .expect("We have called is_numeric on each in char in `c_str`");
                                return Some(Ok(Token {
                                    token_type: TokenType::Number(num),
                                    span: Span {
                                        start: start_byte,
                                        end: self.byte - 1,
                                    },
                                    origin: c_str.to_string(),
                                    line: self.line_num,
                                }));
                            }
                        } else {
                            let num: f64 = c_str
                                .parse()
                                .expect("We have called is_numeric on each in char in `c_str`");
                            return Some(Ok(Token {
                                token_type: TokenType::Number(num),
                                span: Span {
                                    start: start_byte,
                                    end: self.byte - 1,
                                },
                                origin: c_str.to_string(),
                                line: self.line_num,
                            }));
                        }
                    },
                    Started::Identifier => loop {
                        let next_char = chars.next();

                        if let Some((_, cn)) = next_char {
                            if cn == ' ' || cn == '\n' {
                                let token_type = Self::match_reserved_word(c_str);
                                return Some(Ok(Token {
                                    token_type,
                                    span: Span {
                                        start: start_byte,
                                        end: self.byte - 1,
                                    },
                                    origin: c_str.to_string(),
                                    line: self.line_num,
                                }));
                            }
                            if cn.is_alphanumeric() || cn == '_' {
                                c_str = &c_onwards[at..c_str.len() + cn.len_utf8()];
                                self.rest = chars.as_str();
                                self.byte += cn.len_utf8();
                                continue;
                            }
                            let token_type = Self::match_reserved_word(c_str);
                            return Some(Ok(Token {
                                token_type,
                                span: Span {
                                    start: start_byte,
                                    end: self.byte - 1,
                                },
                                origin: c_str.to_string(),
                                line: self.line_num,
                            }));
                        }
                        let token_type = Self::match_reserved_word(c_str);
                        return Some(Ok(Token {
                            token_type,
                            span: Span {
                                start: start_byte,
                                end: self.byte - 1,
                            },
                            origin: c_str.to_string(),
                            line: self.line_num,
                        }));
                    },
                    Started::IfNextEqual(then, else_t) => {
                        if self.rest.starts_with('=') {
                            self.byte += 1;
                            self.rest = &self.rest[1..];
                            let token = Token {
                                token_type: then,
                                span: Span {
                                    start: start_byte,
                                    end: self.byte - 1,
                                },
                                origin: c_str.to_string(),
                                line: self.line_num,
                            };
                            return Some(Ok(token));
                        }
                        let token = Token {
                            token_type: else_t,
                            span: Span {
                                start: start_byte,
                                end: self.byte - 1,
                            },
                            origin: c_str.to_string(),
                            line: self.line_num,
                        };
                        return Some(Ok(token));
                    }
                }
            } else {
                self.at_eof = true;
                return Some(Ok(Token {
                    token_type: TokenType::Eof,
                    span: Span {
                        start: self.byte,
                        end: self.byte,
                    },
                    origin: String::new(),
                    line: self.line_num,
                }));
            }
        }
    }
}
