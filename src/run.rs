//! Run module
//!
//! Responsible for running the AST and returning the computed values
//!

use crate::{
    eval::EvaluatedValue,
    lexer::TokenType,
    parser::{Expr, LiteralAtom, Parser, Stmt},
};

/// `Run`
/// an iterator that consumes expressions from the parser and tries to evaluate them.
pub struct Run<'de> {
    parser: Parser<'de>,
}

impl<'de> Run<'de> {
    /// Create a new `Run` to process a given input source code
    pub fn new(input: &'de str) -> Self {
        Run {
            parser: Parser::new(input),
        }
    }
}

impl Iterator for Run<'_> {
    type Item = Result<(), String>;

    fn next(&mut self) -> Option<Self::Item> {
        let stmt = self.parser.next()?;
        match stmt {
            Ok(s) => Some(evaluate_statement(s)),
            Err(e) => Some(Err(e)),
        }
    }
}

fn evaluate_statement(stmt: Stmt) -> Result<(), String> {
    match stmt {
        Stmt::Print(expr) => {
            let val = evaluate_expression(expr)?;
            println!("{}", val);
        }
        Stmt::ExpressionStatement(expr) => {
            let val = evaluate_expression(expr);
        }
    }
    Ok(())
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
                | TokenType::LessEqual => match (&l_expr, &r_expr) {
                    (EvaluatedValue::Number(_), EvaluatedValue::Number(_)) => {}
                    _ => {
                        eprintln!("Operand must be a number.");
                        eprintln!("[line {}]", operator.line);
                        return Err("Operand must be a number".to_string());
                    }
                },
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
            let r = evaluate_expression(*right);
            if let (TokenType::Minus, Ok(e)) = (operator.token_type, &r) {
                match e {
                    EvaluatedValue::Number(_) => {}
                    _ => {
                        eprintln!("Operand must be a number.");
                        eprintln!("[line {}]", operator.line);
                        return Err("Operand must be a number".to_string());
                    }
                }
            }
            match operator.token_type {
                TokenType::Bang => match &r {
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
                TokenType::Minus => match &r {
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
            LiteralAtom::String(s) => Ok(EvaluatedValue::String(s.to_string())),
            LiteralAtom::Number(num) => Ok(EvaluatedValue::Number(num)),
            LiteralAtom::Nil => Ok(EvaluatedValue::Nil),
            LiteralAtom::Bool(b) => Ok(EvaluatedValue::Bool(b)),
        },
        Expr::Grouping(expr) => evaluate_expression(*expr),
    }
}
