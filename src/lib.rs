//! Loxide
//!
//! The Lexing and Parsing logic to implement a interpreter for the Lox programming language
//!
//! As defined in <https://craftinginterpreters.com>

#![allow(clippy::float_cmp)]
#![allow(clippy::too_many_lines)]
#![warn(missing_docs)]
mod builtins;
pub mod eval;
pub mod eval_parser;
pub mod interpreter;
pub mod lexer;
mod parser;
mod value;
