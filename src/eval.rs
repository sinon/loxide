//! Eval module
//!
//! Responsible for evalulating the AST and returning the computed values
//! Only supports simple expressions

use crate::{
    eval_parser::{Expr, LiteralAtom, Parser},
    lexer::TokenType,
    value::EvaluatedValue,
};

/// `Eval`
/// an iterator that consumes expressions from the parser and tries to evaluate them.
pub struct Eval<'de> {
    parser: Parser<'de>,
}

impl<'de> Eval<'de> {
    /// Create a new `Eval` to process a given input source code
    #[must_use]
    pub fn new(input: &'de str) -> Self {
        Eval {
            parser: Parser::new(input),
        }
    }
}

impl Iterator for Eval<'_> {
    type Item = Result<EvaluatedValue, String>;

    fn next(&mut self) -> Option<Self::Item> {
        let expr = self.parser.next()?;
        expr.map_or_else(|_| todo!(), |e| Some(evaluate_expression(e)))
    }
}
fn evaluate_expression(expr: Expr) -> Result<EvaluatedValue, String> {
    match expr {
        Expr::Binary {
            left,
            operator,
            right,
        } => {
            let l_expr = evaluate_expression(*left)?;
            let r_expr = evaluate_expression(*right)?;
            match operator.token_type {
                TokenType::Minus
                | TokenType::Star
                | TokenType::Slash
                | TokenType::Greater
                | TokenType::GreaterEqual
                | TokenType::Less
                | TokenType::LessEqual => {
                    if let (EvaluatedValue::Number(_), EvaluatedValue::Number(_)) =
                        (&l_expr, &r_expr)
                    {
                    } else {
                        eprintln!("Operand must be a number.");
                        eprintln!("[line {}]", operator.line);
                        return Err("Operand must be a number".to_string());
                    }
                }
                TokenType::Plus => match (&l_expr, &r_expr) {
                    (EvaluatedValue::Number(_), EvaluatedValue::Number(_))
                    | (EvaluatedValue::String(_), EvaluatedValue::String(_)) => {}
                    _ => {
                        eprintln!("Operand must be a number.");
                        eprintln!("[line {}]", operator.line);
                        return Err("Operand must be a number".to_string());
                    }
                },
                _ => {}
            }

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
                        _ => panic!("{op} is not a valid token type for Expr::Binary with Numbers"),
                    }
                }
                (EvaluatedValue::String(s1), EvaluatedValue::String(s2), operator) => {
                    match operator.token_type {
                        TokenType::Plus => Ok(EvaluatedValue::String(s1 + &s2)),
                        TokenType::EqualEqual => Ok(EvaluatedValue::Bool(s1 == s2)),
                        TokenType::BangEqual => Ok(EvaluatedValue::Bool(s1 != s2)),
                        // TODO: Make unrepresentable by narrowing `operator` to `BinaryOperator:Not|Negate`
                        _ => panic!(
                            "{operator} is not a valid token type for Expr:Binary with Strings"
                        ),
                    }
                }
                (EvaluatedValue::String(_), EvaluatedValue::Number(_), operator)
                | (EvaluatedValue::Number(_), EvaluatedValue::String(_), operator) => {
                    match operator.token_type {
                        TokenType::EqualEqual => Ok(EvaluatedValue::Bool(false)),
                        TokenType::BangEqual => Ok(EvaluatedValue::Bool(true)),
                        _ => panic!("{operator} is not supported for String<>Number"),
                    }
                }
                (l, r, op) => todo!("Add handling for {l} {r} {op}"),
            }
        }
        Expr::Unary { operator, right } => {
            let r = evaluate_expression(*right);
            if let (TokenType::Minus, Ok(e)) = (operator.token_type, &r) {
                if let EvaluatedValue::Number(_) = e {
                } else {
                    eprintln!("Operand must be a number.");
                    eprintln!("[line {}]", operator.line);
                    return Err("Operand must be a number".to_string());
                }
            }
            match operator.token_type {
                TokenType::Bang => r.as_ref().map_or_else(
                    |_| todo!(),
                    |v| match v {
                        EvaluatedValue::String(_) | EvaluatedValue::Number(_) => {
                            Ok(EvaluatedValue::Bool(false))
                        }
                        EvaluatedValue::Nil => Ok(EvaluatedValue::Bool(true)),
                        EvaluatedValue::Bool(b) => match b {
                            true => Ok(EvaluatedValue::Bool(false)),
                            false => Ok(EvaluatedValue::Bool(true)),
                        },
                        _ => todo!(),
                    },
                ),
                TokenType::Minus => r.as_ref().map_or_else(
                    |_| todo!(),
                    |v| match v {
                        EvaluatedValue::Number(n) => Ok(EvaluatedValue::Number(-n)),
                        _ => todo!(),
                    },
                ),
                // TODO: Make unrepresentable by narrowing `operator` to `UnaryOperator:Not|Negate`
                _ => {
                    panic!("{:?} is not a valid unary token type", operator.token_type)
                }
            }
        }
        Expr::Literal(literal_atom) => match literal_atom {
            LiteralAtom::String(s) => Ok(EvaluatedValue::String(s.to_string())),
            LiteralAtom::Number(num) => Ok(EvaluatedValue::Number(num)),
            LiteralAtom::Nil => Ok(EvaluatedValue::Nil),
            LiteralAtom::Bool(b) => Ok(EvaluatedValue::Bool(b)),
        },
        Expr::Grouping(expr) => evaluate_expression(*expr),
    }
}
