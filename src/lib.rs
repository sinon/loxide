//! Loxide
//!
//! The Lexing and Parsing logic to implement a interpreter for the Lox programming language
//!
//! As defined in <https://craftinginterpreters.com>

#![warn(missing_docs)]
pub mod eval;
pub mod eval_parser;
pub mod lexer;
pub mod parser;
pub mod run;
