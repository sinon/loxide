//! Eval module
//!
//! Responsible for evlauting the AST and returning the computed values
//!

use std::fmt::Display;

use crate::{
    lexer::TokenType,
    parser::{Expr, LiteralAtom, Parser},
};

pub enum EvaluatedValue {
    String(String),
    Number(f64),
    Nil,
    Bool(bool),
}

impl Display for EvaluatedValue {
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
    type Item = Result<EvaluatedValue, String>;

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
    fn evaluate_expression(&self, expr: Expr<'de>) -> Option<Result<EvaluatedValue, String>> {
        match expr {
            Expr::Binary {
                left,
                operator,
                right,
            } => {
                let l_expr = self.evaluate_expression(*left)?;
                let r_expr = self.evaluate_expression(*right)?;
                let l_expr = l_expr.expect("just cos");
                let r_expr = r_expr.expect("just cos 2");
                match (l_expr, r_expr, operator) {
                    (EvaluatedValue::Number(n1), EvaluatedValue::Number(n2), op) => {
                        match op.token_type {
                            TokenType::Plus => Some(Ok(EvaluatedValue::Number(n1 + n2))),
                            TokenType::Minus => Some(Ok(EvaluatedValue::Number(n1 - n2))),
                            TokenType::Star => Some(Ok(EvaluatedValue::Number(n1 * n2))),
                            TokenType::Slash => Some(Ok(EvaluatedValue::Number(n1 / n2))),
                            // TODO: Make unrepresentable by narrowing `operator` to `BinaryOperator:Not|Negate`
                            _ => panic!(
                                "{} is not a valid token type for Expr::Binary with Numbers",
                                op
                            ),
                        }
                    }
                    (EvaluatedValue::String(s1), EvaluatedValue::String(s2), operator) => {
                        match operator.token_type {
                            TokenType::Plus => {
                                // let s3 = &((s1.to_owned() + s2).clone());
                                Some(Ok(EvaluatedValue::String(s1.to_owned() + &s2)))
                            }
                            // TODO: Make unrepresentable by narrowing `operator` to `BinaryOperator:Not|Negate`
                            _ => panic!(
                                "{} is not a valid token type for Expr:Binary with Strings",
                                operator
                            ),
                        }
                    }
                    (l, r, op) => todo!("Add handling for {l} {r} {op}"),
                }
            }
            Expr::Unary { operator, right } => match operator.token_type {
                TokenType::Bang => match self.evaluate_expression(*right)? {
                    Ok(v) => match v {
                        EvaluatedValue::String(_) => Some(Ok(EvaluatedValue::Bool(false))),
                        EvaluatedValue::Number(_) => Some(Ok(EvaluatedValue::Bool(false))),
                        EvaluatedValue::Nil => Some(Ok(EvaluatedValue::Bool(true))),
                        EvaluatedValue::Bool(b) => match b {
                            true => Some(Ok(EvaluatedValue::Bool(false))),
                            false => Some(Ok(EvaluatedValue::Bool(true))),
                        },
                    },
                    Err(_) => todo!(),
                },
                TokenType::Minus => match self.evaluate_expression(*right)? {
                    Ok(v) => match v {
                        EvaluatedValue::String(_) => todo!(),
                        EvaluatedValue::Number(n) => Some(Ok(EvaluatedValue::Number(-n))),
                        EvaluatedValue::Nil => todo!(),
                        EvaluatedValue::Bool(_) => todo!(),
                    },
                    Err(_) => todo!(),
                },
                // TODO: Make unrepresentable by narrowing `operator` to `UnaryOperator:Not|Negate`
                _ => {
                    panic!("{:?} is not a valid unary token type", operator.token_type)
                }
            },
            Expr::Literal(literal_atom) => match literal_atom {
                LiteralAtom::String(s) => {
                    return Some(Ok(EvaluatedValue::String(s.to_string())));
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
