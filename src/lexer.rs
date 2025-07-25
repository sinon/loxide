//! Lexer
//!
//! Responsible for transforming a given input str into a Iterator of `Result<Token>`

use std::{fmt, process::ExitCode};

use miette::{Error, LabeledSpan, Result, miette};

/// `Token` is formed of a token type (`TokenType`) and a reference to a str in the input string
#[derive(Clone, Debug)]
pub struct Token<'de> {
    /// The `TokenType` of `Token`
    pub token_type: TokenType,
    /// The text reference from the source code
    pub origin: &'de str,
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

impl fmt::Display for Token<'_> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let i = self.origin;
        match self.token_type {
            TokenType::RightParen => write!(f, "RIGHT_PAREN {i} null"),
            TokenType::LeftParen => write!(f, "LEFT_PAREN {i} null"),
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
            TokenType::And => write!(f, "AND {i} null"),
            TokenType::Class => write!(f, "CLASS {i} null"),
            TokenType::Else => write!(f, "ELSE {i} null"),
            TokenType::False => write!(f, "FALSE {i} null"),
            TokenType::Fun => write!(f, "FUN {i} null"),
            TokenType::For => write!(f, "FOR {i} null"),
            TokenType::If => write!(f, "IF {i} null"),
            TokenType::Nil => write!(f, "NIL {i} null"),
            TokenType::Or => write!(f, "OR {i} null"),
            TokenType::Print => write!(f, "PRINT {i} null"),
            TokenType::Return => write!(f, "RETURN {i} null"),
            TokenType::Super => write!(f, "SUPER {i} null"),
            TokenType::This => write!(f, "THIS {i} null"),
            TokenType::True => write!(f, "TRUE {i} null"),
            TokenType::Var => write!(f, "VAR {i} null"),
            TokenType::While => write!(f, "WHILE {i} null"),
            TokenType::Eof => write!(f, "EOF  null"),
        }
    }
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
    /// Start lexing on `Lexer` used by `tokenize` command.
    pub fn tokenize_lex(&mut self) -> ExitCode {
        let mut exit_code = 0;
        for t in self {
            match t {
                Ok(token) => println!("{token}"),
                Err(e) => {
                    eprintln!("{e}");
                    exit_code = 65;
                }
            }
        }
        ExitCode::from(exit_code)
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

impl<'de> Iterator for Lexer<'de> {
    type Item = Result<Token<'de>, Error>;

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
                self.byte += c.len_utf8();

                let res = move |t_type: TokenType, line_num: usize| {
                    Some(Ok(Token {
                        token_type: t_type,
                        origin: c_str,
                        line: line_num,
                    }))
                };

                let started = match c {
                '(' => return res(TokenType::LeftParen, self.line_num),
                ')' => return res(TokenType::RightParen, self.line_num),
                '{' => return res(TokenType::LeftBrace, self.line_num),
                '}' => return res(TokenType::RightBrace, self.line_num),
                ',' => return res(TokenType::Comma, self.line_num),
                '.' => return res(TokenType::Dot, self.line_num),
                '-' => return res(TokenType::Minus, self.line_num),
                '+' => return res(TokenType::Plus, self.line_num),
                '*' => return res(TokenType::Star, self.line_num),
                ';' => return res(TokenType::Semicolon, self.line_num),
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
                                origin: c_str,
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
                                origin: c_str,
                                line: self.line_num,
                            }));
                        }
                        // Scan to end, we cannot continue to scan for tokens when in an unterminated string
                        self.byte = self.whole.len();
                        self.rest = &self.rest[self.rest.len()..self.rest.len()];
                        return Some(Err(
                            miette! {labels = vec![LabeledSpan::at(self.byte -c.len_utf8()..self.byte, "this unterminated string")],
                            "[line {}] Error: Unterminated string.", self.line_num}.with_source_code(self.whole.to_string())
                        ));
                    }
                    Started::Number => {
                        loop {
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
                                            c_str = &c_onwards[at..c_str.len()
                                                + cn.len_utf8()
                                                + c_peek.len_utf8()];
                                            self.rest = chars.as_str();
                                            self.byte += cn.len_utf8() + c_peek.len_utf8();
                                        } else {
                                            let num = c_str.parse().expect(
                                                "We have called is_numeric on each char in `c_str`",
                                            );
                                            return Some(Ok(Token {
                                                token_type: TokenType::Number(num),
                                                origin: c_str,
                                                line: self.line_num,
                                            }));
                                        }
                                    }
                                } else {
                                    let num: f64 = c_str.parse().expect(
                                        "We have called is_numeric on each in char in `c_str`",
                                    );
                                    return Some(Ok(Token {
                                        token_type: TokenType::Number(num),
                                        origin: c_str,
                                        line: self.line_num,
                                    }));
                                }
                            } else {
                                let num: f64 = c_str
                                    .parse()
                                    .expect("We have called is_numeric on each in char in `c_str`");
                                return Some(Ok(Token {
                                    token_type: TokenType::Number(num),
                                    origin: c_str,
                                    line: self.line_num,
                                }));
                            }
                        }
                    }
                    Started::Identifier => loop {
                        let next_char = chars.next();

                        if let Some((_, cn)) = next_char {
                            if cn == ' ' || cn == '\n' {
                                let token_type = Self::match_reserved_word(c_str);
                                return Some(Ok(Token {
                                    token_type,
                                    origin: c_str,
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
                                origin: c_str,
                                line: self.line_num,
                            }));
                        }
                        let token_type = Self::match_reserved_word(c_str);
                        return Some(Ok(Token {
                            token_type,
                            origin: c_str,
                            line: self.line_num,
                        }));
                    },
                    Started::IfNextEqual(then, else_t) => {
                        if self.rest.starts_with('=') {
                            let c_str = &c_onwards[..2];
                            self.byte += 1;
                            self.rest = &self.rest[1..];
                            let token = Token {
                                token_type: then,
                                origin: c_str,
                                line: self.line_num,
                            };
                            return Some(Ok(token));
                        }
                        let token = Token {
                            token_type: else_t,
                            origin: c_str,
                            line: self.line_num,
                        };
                        return Some(Ok(token));
                    }
                }
            } else {
                self.at_eof = true;
                return Some(Ok(Token {
                    token_type: TokenType::Eof,
                    origin: "",
                    line: self.line_num,
                }));
            }
        }
    }
}
