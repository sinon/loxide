//! Eval module
//!
//! Responsible for evlauting the AST and returning the computed values
//!

use std::fmt::Display;

use crate::{
    eval_parser::{Expr, LiteralAtom, Parser},
    interpreter::NativeFunction,
    lexer::TokenType,
};

/// The value that an expression has evaluated too, this can be a literal.
#[derive(Clone, Debug)]
pub enum EvaluatedValue {
    /// String value `"hello"`
    String(String),
    /// Number value. Note Lox only supports double precision floating point
    Number(f64),
    /// nil, the unset/null value
    Nil,
    /// Boolean value `true`/`false`
    Bool(bool),
    /// fn
    NativeFunction(NativeFunction),
}

impl EvaluatedValue {
    pub(crate) const fn is_truthy(&self) -> bool {
        match self {
            Self::String(_) | Self::Number(_) => true,
            Self::Nil => false,
            Self::Bool(b) => *b,
            Self::NativeFunction(_f) => true,
        }
    }
}

impl From<EvaluatedValue> for bool {
    fn from(val: EvaluatedValue) -> Self {
        match val {
            EvaluatedValue::String(_) | EvaluatedValue::Number(_) => true,
            EvaluatedValue::Nil => false,
            EvaluatedValue::Bool(b) => b,
            EvaluatedValue::NativeFunction(_f) => true,
        }
    }
}

impl Display for EvaluatedValue {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::String(s) => write!(f, "{s}"),
            Self::Number(n) => write!(f, "{n}"),
            Self::Nil => write!(f, "nil"),
            Self::Bool(b) => write!(f, "{b:}"),
            Self::NativeFunction(_native_fn) => write!(f, "nat fn # TODO"),
        }
    }
}

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
                        EvaluatedValue::NativeFunction(_f) => todo!(),
                    },
                ),
                TokenType::Minus => r.as_ref().map_or_else(
                    |_| todo!(),
                    |v| match v {
                        EvaluatedValue::String(_) => todo!(),
                        EvaluatedValue::Number(n) => Ok(EvaluatedValue::Number(-n)),
                        EvaluatedValue::Nil => todo!(),
                        EvaluatedValue::Bool(_) => todo!(),
                        EvaluatedValue::NativeFunction(_) => todo!(),
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
