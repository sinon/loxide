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
            Ok(e) => return Some(self.evaluate_expression(e)),
            Err(_) => todo!(),
        }
        todo!()
    }
}
impl<'de> Eval<'de> {
    fn evaluate_expression(&self, expr: Expr<'de>) -> Result<EvaluatedValue, String> {
        match expr {
            Expr::Binary {
                left,
                operator,
                right,
            } => {
                let l_expr = self.evaluate_expression(*left)?;
                let r_expr = self.evaluate_expression(*right)?;
                match (l_expr, r_expr, operator) {
                    (EvaluatedValue::Number(n1), EvaluatedValue::Number(n2), op) => {
                        match op.token_type {
                            TokenType::Plus => Ok(EvaluatedValue::Number(n1 + n2)),
                            TokenType::Minus => Ok(EvaluatedValue::Number(n1 - n2)),
                            TokenType::Star => Ok(EvaluatedValue::Number(n1 * n2)),
                            TokenType::Slash => Ok(EvaluatedValue::Number(n1 / n2)),
                            TokenType::Greater => Ok(EvaluatedValue::Bool(n1 > n2)),
                            TokenType::GreaterEqual => Ok(EvaluatedValue::Bool(n1 >= n2)),
                            TokenType::Less => Ok(EvaluatedValue::Bool(n1 < n2)),
                            TokenType::LessEqual => Ok(EvaluatedValue::Bool(n1 <= n2)),
                            TokenType::EqualEqual => Ok(EvaluatedValue::Bool(n1 == n2)),
                            TokenType::BangEqual => Ok(EvaluatedValue::Bool(n1 != n2)),
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
                                Ok(EvaluatedValue::String(s1.to_owned() + &s2))
                            }
                            TokenType::EqualEqual => Ok(EvaluatedValue::Bool(s1 == s2)),
                            TokenType::BangEqual => Ok(EvaluatedValue::Bool(s1 != s2)),
                            // TODO: Make unrepresentable by narrowing `operator` to `BinaryOperator:Not|Negate`
                            _ => panic!(
                                "{} is not a valid token type for Expr:Binary with Strings",
                                operator
                            ),
                        }
                    }
                    (EvaluatedValue::String(_), EvaluatedValue::Number(_), operator)
                    | (EvaluatedValue::Number(_), EvaluatedValue::String(_), operator) => {
                        match operator.token_type {
                            TokenType::EqualEqual => Ok(EvaluatedValue::Bool(false)),
                            TokenType::BangEqual => Ok(EvaluatedValue::Bool(true)),
                            _ => panic!("{} is not supported for String<>Number", operator),
                        }
                    }
                    (l, r, op) => todo!("Add handling for {l} {r} {op}"),
                }
            }
            Expr::Unary { operator, right } => {
                let r = *right;
                match (operator.token_type, &r) {
                    (TokenType::Minus, Expr::Literal(literal_atom)) => match literal_atom {
                        LiteralAtom::Number(_) => {}
                        _ => {
                            eprintln!("Operand must be a number.");
                            eprintln!("[line {}]", operator.line);
                            return Err("Operand must be a number".to_string());
                        }
                    },
                    _ => {}
                }
                match operator.token_type {
                    TokenType::Bang => match self.evaluate_expression(r) {
                        Ok(v) => match v {
                            EvaluatedValue::String(_) => Ok(EvaluatedValue::Bool(false)),
                            EvaluatedValue::Number(_) => Ok(EvaluatedValue::Bool(false)),
                            EvaluatedValue::Nil => Ok(EvaluatedValue::Bool(true)),
                            EvaluatedValue::Bool(b) => match b {
                                true => Ok(EvaluatedValue::Bool(false)),
                                false => Ok(EvaluatedValue::Bool(true)),
                            },
                        },
                        Err(_) => todo!(),
                    },
                    TokenType::Minus => match self.evaluate_expression(r) {
                        Ok(v) => match v {
                            EvaluatedValue::String(_) => todo!(),
                            EvaluatedValue::Number(n) => Ok(EvaluatedValue::Number(-n)),
                            EvaluatedValue::Nil => todo!(),
                            EvaluatedValue::Bool(_) => todo!(),
                        },
                        Err(_) => todo!(),
                    },
                    // TODO: Make unrepresentable by narrowing `operator` to `UnaryOperator:Not|Negate`
                    _ => {
                        panic!("{:?} is not a valid unary token type", operator.token_type)
                    }
                }
            }
            Expr::Literal(literal_atom) => match literal_atom {
                LiteralAtom::String(s) => {
                    return Ok(EvaluatedValue::String(s.to_string()));
                }
                LiteralAtom::Number(num) => return Ok(EvaluatedValue::Number(num)),
                LiteralAtom::Nil => return Ok(EvaluatedValue::Nil),
                LiteralAtom::Bool(b) => {
                    return Ok(EvaluatedValue::Bool(b));
                }
            },
            Expr::Grouping(expr) => self.evaluate_expression(*expr),
        }
    }
}
