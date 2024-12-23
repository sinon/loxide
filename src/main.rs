use core::fmt;
use std::env;
use std::fs;
use std::io::{self, Write};
use std::process::ExitCode;

#[derive(PartialEq, Eq)]
enum Atom {
    RightParen,
    LeftParen,
    RightBrace,
    LeftBrace,
    Comma,
    Dot,
    Minus,
    Plus,
    Star,
    SemiColon,
    Eof,
    Equals,
    EqualEquals,
    Bang,
    BangEquals,
    Less,
    LessEquals,
    Greater,
    GreaterEquals,
    Slash,
}

impl fmt::Debug for Atom {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Atom::RightParen => write!(f, "RIGHT_PAREN ) null"),
            Atom::LeftParen => write!(f, "LEFT_PAREN ( null"),
            Atom::Eof => write!(f, "EOF  null"),
            Atom::RightBrace => write!(f, "RIGHT_BRACE }} null"),
            Atom::LeftBrace => write!(f, "LEFT_BRACE {{ null"),
            Atom::Comma => write!(f, "COMMA , null"),
            Atom::Dot => write!(f, "DOT . null"),
            Atom::Minus => write!(f, "MINUS - null"),
            Atom::Plus => write!(f, "PLUS + null"),
            Atom::Star => write!(f, "STAR * null"),
            Atom::SemiColon => write!(f, "SEMICOLON ; null"),
            Atom::Equals => write!(f, "EQUAL = null"),
            Atom::EqualEquals => write!(f, "EQUAL_EQUAL == null"),
            Atom::Bang => write!(f, "BANG ! null"),
            Atom::BangEquals => write!(f, "BANG_EQUAL != null"),
            Atom::Less => write!(f, "LESS < null"),
            Atom::LessEquals => write!(f, "LESS_EQUAL <= null"),
            Atom::Greater => write!(f, "GREATER > null"),
            Atom::GreaterEquals => write!(f, "GREATER_EQUAL >= null"),
            Atom::Slash => write!(f, "SLASH / null"),
        }
    }
}

/*
LEFT_PAREN ( null
LEFT_PAREN ( null
RIGHT_PAREN ) null
EOF  null
 */

fn main() -> ExitCode {
    let args: Vec<String> = env::args().collect();
    if args.len() < 3 {
        writeln!(io::stderr(), "Usage: {} tokenize <filename>", args[0]).unwrap();
        return ExitCode::FAILURE;
    }

    let command = &args[1];
    let filename = &args[2];

    match command.as_str() {
        "tokenize" => {
            let file_contents = fs::read_to_string(filename).unwrap_or_else(|_| {
                writeln!(io::stderr(), "Failed to read file {}", filename).unwrap();
                String::new()
            });
            let mut return_code = 0;
            if !file_contents.is_empty() {
                for (ln_num, ln) in file_contents.lines().enumerate() {
                    let mut char_iter = ln.chars().peekable();
                    while let Some(c) = char_iter.next() {
                        match c {
                            ' ' | '\t' => {
                                continue;
                            }
                            '(' => println!("{:?}", Atom::LeftParen),
                            ')' => println!("{:?}", Atom::RightParen),
                            '{' => println!("{:?}", Atom::LeftBrace),
                            '}' => println!("{:?}", Atom::RightBrace),
                            ',' => println!("{:?}", Atom::Comma),
                            '.' => println!("{:?}", Atom::Dot),
                            '-' => println!("{:?}", Atom::Minus),
                            '+' => println!("{:?}", Atom::Plus),
                            '*' => println!("{:?}", Atom::Star),
                            ';' => println!("{:?}", Atom::SemiColon),
                            '=' | '!' | '>' | '<' | '/' => {
                                let (current_atom, peek_check_ahead, two_char_atom) = match c {
                                    '=' => (Atom::Equals, '=', Atom::EqualEquals),
                                    '!' => (Atom::Bang, '=', Atom::BangEquals),
                                    '>' => (Atom::Greater, '=', Atom::GreaterEquals),
                                    '<' => (Atom::Less, '=', Atom::LessEquals),
                                    '/' => (Atom::Slash, '/', Atom::Slash),
                                    _ => panic!(),
                                };
                                if let Some(n) = char_iter.peek() {
                                    if *n == peek_check_ahead && two_char_atom != Atom::Slash {
                                        println!("{:?}", two_char_atom);
                                        char_iter.next();
                                    } else if *n == peek_check_ahead && two_char_atom == Atom::Slash
                                    {
                                        break;
                                    } else {
                                        println!("{:?}", current_atom)
                                    }
                                } else {
                                    println!("{:?}", current_atom)
                                }
                            }
                            _ => {
                                eprintln!(
                                    "[line {}] Error: Unexpected character: {}",
                                    ln_num + 1,
                                    c
                                );
                                return_code = 65;
                            }
                        }
                    }
                }
            }
            println!("{:?}", Atom::Eof);
            return ExitCode::from(return_code);
        }
        _ => {
            writeln!(io::stderr(), "Unknown command: {}", command).unwrap();
            return ExitCode::FAILURE;
        }
    }
}
