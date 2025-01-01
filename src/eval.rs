//! Eval module
//!
//! Responsible for evlauting the AST and returning the computed values
//!

use std::fmt::Display;

use crate::parser::{Expr, LiteralAtom, Parser};

pub enum EvaltuatedValue<'de> {
    String(&'de str),
    Number(f64),
    Nil,
    Bool(bool),
}

impl Display for EvaltuatedValue<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            EvaltuatedValue::String(s) => write!(f, "{s}"),
            EvaltuatedValue::Number(n) => write!(f, "{n}"),
            EvaltuatedValue::Nil => write!(f, "nil"),
            EvaltuatedValue::Bool(b) => write!(f, "{b:}"),
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
    type Item = Result<EvaltuatedValue<'de>, String>;

    fn next(&mut self) -> Option<Self::Item> {
        let expr = self.parser.next()?;

        match expr {
            Ok(e) => match e {
                Expr::Binary {
                    left,
                    operator,
                    right,
                } => {}
                Expr::Unary { operator, right } => todo!(),
                Expr::Literal(literal_atom) => match literal_atom {
                    LiteralAtom::String(s) => {
                        return Some(Ok(EvaltuatedValue::String(s)));
                    }
                    LiteralAtom::Number(num) => return Some(Ok(EvaltuatedValue::Number(num))),
                    LiteralAtom::Nil => return Some(Ok(EvaltuatedValue::Nil)),
                    LiteralAtom::Bool(b) => {
                        return Some(Ok(EvaltuatedValue::Bool(b)));
                    }
                },
                Expr::Grouping(expr) => todo!(),
            },
            Err(_) => todo!(),
        }
        todo!()
    }
}
