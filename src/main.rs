use core::fmt;
use std::env;
use std::fs;
use std::io::{self, Write};

enum Atom {
    RightParen,
    LeftParen,
    Eof,
}

impl fmt::Debug for Atom {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Atom::RightParen => write!(f, "RIGHT_PAREN ) null"),
            Atom::LeftParen => write!(f, "LEFT_PAREN ( null"),
            Atom::Eof => write!(f, "EOF  null"),
        }
    }
}

/*
LEFT_PAREN ( null
LEFT_PAREN ( null
RIGHT_PAREN ) null
EOF  null
 */

fn main() {
    let args: Vec<String> = env::args().collect();
    if args.len() < 3 {
        writeln!(io::stderr(), "Usage: {} tokenize <filename>", args[0]).unwrap();
        return;
    }

    let command = &args[1];
    let filename = &args[2];

    match command.as_str() {
        "tokenize" => {
            let file_contents = fs::read_to_string(filename).unwrap_or_else(|_| {
                writeln!(io::stderr(), "Failed to read file {}", filename).unwrap();
                String::new()
            });

            if !file_contents.is_empty() {
                for ln in file_contents.lines() {
                    for c in ln.chars() {
                        match c {
                            '(' => println!("{:?}", Atom::LeftParen),
                            ')' => println!("{:?}", Atom::RightParen),
                            _ => {}
                        }
                    }
                }
                println!("{:?}", Atom::Eof)
            }
        }
        _ => {
            writeln!(io::stderr(), "Unknown command: {}", command).unwrap();
            return;
        }
    }
}
