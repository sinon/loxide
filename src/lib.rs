//! Loxide
//!
//! The Lexing and Parsing logic to implement a interpreter for the Lox programming language
//!
//! As defined in <https://craftinginterpreters.com>

#![allow(clippy::float_cmp)]
#![allow(clippy::too_many_lines)]
#![warn(missing_docs)]
pub mod eval;
pub mod eval_parser;
pub mod lexer;
pub mod parser;
pub mod run;
