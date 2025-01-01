//! Eval module
//!
//! Responsible for evlauting the AST and returning the computed values
//!

use std::fmt::Display;

use crate::parser::{Expr, LiteralAtom, Parser};

pub enum EvaluatedValue<'de> {
    String(&'de str),
    Number(f64),
    Nil,
    Bool(bool),
}

impl Display for EvaluatedValue<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            EvaluatedValue::String(s) => write!(f, "{s}"),
            EvaluatedValue::Number(n) => write!(f, "{n}"),
            EvaluatedValue::Nil => write!(f, "nil"),
            EvaluatedValue::Bool(b) => write!(f, "{b:}"),
        }
    }
}

pub struct Eval<'de> {
    parser: Parser<'de>,
}

impl<'de> Eval<'de> {
    pub fn new(input: &'de str) -> Self {
        Eval {
            parser: Parser::new(input),
        }
    }
}

impl<'de> Iterator for Eval<'de> {
    type Item = Result<EvaluatedValue<'de>, String>;

    fn next(&mut self) -> Option<Self::Item> {
        let expr = self.parser.next()?;

        match expr {
            Ok(e) => return self.evaluate_expression(e),
            Err(_) => todo!(),
        }
        todo!()
    }
}
impl<'de> Eval<'de> {
    fn evaluate_expression(&self, expr: Expr<'de>) -> Option<Result<EvaluatedValue<'de>, String>> {
        match expr {
            Expr::Binary {
                left,
                operator,
                right,
            } => {
                todo!()
            }
            Expr::Unary { operator, right } => todo!(),
            Expr::Literal(literal_atom) => match literal_atom {
                LiteralAtom::String(s) => {
                    return Some(Ok(EvaluatedValue::String(s)));
                }
                LiteralAtom::Number(num) => return Some(Ok(EvaluatedValue::Number(num))),
                LiteralAtom::Nil => return Some(Ok(EvaluatedValue::Nil)),
                LiteralAtom::Bool(b) => {
                    return Some(Ok(EvaluatedValue::Bool(b)));
                }
            },
            Expr::Grouping(expr) => self.evaluate_expression(*expr),
        }
    }
}
