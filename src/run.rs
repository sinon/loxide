//! Run module
//!
//! Responsible for running the AST and returning the computed values
//!

use std::collections::HashMap;

use crate::{
    eval::EvaluatedValue,
    lexer::TokenType,
    parser::{Expr, LiteralAtom, Parser, Stmt},
};

/// `Run`
/// an iterator that consumes expressions from the parser and tries to evaluate them.
pub struct Run<'de> {
    parser: Parser<'de>,
    environment: Environment<'de>,
}

#[derive(Debug, Clone)]
struct Environment<'de> {
    data: HashMap<&'de str, EvaluatedValue>,
    enclosing: Option<Box<Environment<'de>>>,
}

impl<'de> Environment<'de> {
    fn new() -> Self {
        Environment {
            data: HashMap::new(),
            enclosing: None,
        }
    }

    fn from_parent(enclosing: &Environment<'de>) -> Self {
        Environment {
            data: HashMap::new(),
            enclosing: Some(Box::new(enclosing.clone())),
        }
    }

    fn get(&self, key: &'de str) -> Option<EvaluatedValue> {
        match &self.enclosing {
            Some(env) => match env.get(key) {
                Some(v) => Some(v.clone()),
                None => None,
            },
            None => match self.data.get(key) {
                Some(v) => Some(v.clone()),
                None => None,
            },
        }
    }

    fn assign(&mut self, key: &'de str, value: &EvaluatedValue) -> Result<(), String> {
        match &mut self.enclosing {
            Some(env) => {
                env.assign(key, value)?;
                Ok(())
            }
            None => {
                self.data.insert(key, value.clone());
                Ok(())
            }
        }
    }
}

impl<'de> Run<'de> {
    /// Create a new `Run` to process a given input source code
    pub fn new(input: &'de str) -> Self {
        Run {
            parser: Parser::new(input),
            environment: Environment::new(),
        }
    }
}

impl Iterator for Run<'_> {
    type Item = Result<(), u8>;

    fn next(&mut self) -> Option<Self::Item> {
        let stmt = self.parser.next()?;
        match stmt {
            Ok(s) => {
                let eval_stmt = evaluate_statement(s, &mut self.environment);
                match eval_stmt {
                    Ok(_) => Some(Ok(())),
                    Err(_) => Some(Err(70)),
                }
            }
            Err(_) => Some(Err(65)),
        }
    }
}

fn evaluate_statement<'de>(
    stmt: Stmt<'de>,
    environment: &mut Environment<'de>,
) -> Result<(), String> {
    match stmt {
        Stmt::Print(expr) => {
            let val = evaluate_expression(expr, environment)?;
            println!("{}", val);
        }
        Stmt::ExpressionStatement(expr) => {
            evaluate_expression(expr, environment)?;
        }
        Stmt::Var(name, expr) => match expr {
            Some(v) => {
                let evalutated_val = evaluate_expression(v, environment)?;
                environment.assign(name, &evalutated_val)?;
            }
            None => {
                environment.assign(name, &EvaluatedValue::Nil)?;
            }
        },
        Stmt::Block(stmts) => {
            // TODO: Bug where Expr::Assign mutations to enclosing env are lost
            // See `run_error_undefined_var.lox` for failure triggering source code example
            let mut new_env = Environment::from_parent(environment);
            for stmt in stmts {
                evaluate_statement(stmt, &mut new_env)?;
            }
        }
    }
    Ok(())
}

fn evaluate_expression<'de>(
    expr: Expr<'de>,
    environment: &mut Environment<'de>,
) -> Result<EvaluatedValue, String> {
    match expr {
        Expr::Binary {
            left,
            operator,
            right,
        } => {
            let l_expr = evaluate_expression(*left, environment)?;
            let r_expr = evaluate_expression(*right, environment)?;
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
                (EvaluatedValue::Bool(b1), EvaluatedValue::Bool(b2), operator) => {
                    match operator.token_type {
                        TokenType::BangEqual => Ok(EvaluatedValue::Bool(b1 != b2)),
                        TokenType::EqualEqual => Ok(EvaluatedValue::Bool(b1 == b2)),
                        _ => panic!("{} is not for suppoer Bool / Bool binary", operator),
                    }
                }
                (l, r, op) => todo!("Add handling for {l} {r} {op}"),
            }
        }
        Expr::Unary { operator, right } => {
            let r = evaluate_expression(*right, environment);
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
        Expr::Grouping(expr) => evaluate_expression(*expr, environment),
        Expr::Variable(token) => match environment.get(token.origin) {
            Some(v) => Ok(v),
            None => {
                eprintln!("Undefined variable '{}'.", token.origin);
                eprintln!("[line {}]", token.line);
                Err("Undefined var".to_string())
            }
        },
        Expr::Assign(name, expr) => match environment.get(name) {
            Some(_) => {
                let eval_expr = evaluate_expression(*expr, environment)?;
                // dbg!("ReAssigning: {name} to '{eval_expr}'");
                // dbg!("{environment:?}");
                // dbg!("{:?}", &environment.enclosing);
                // if let Some(enc) = &mut environment.enclosing {
                //     enc.assign(name, &eval_expr)?;
                // } else {
                //     environment.assign(name, &eval_expr)?;
                // }
                // environment
                //     .enclosing
                //     .as_mut()
                //     .unwrap()
                //     .assign(name, &eval_expr)?;
                environment.assign(name, &eval_expr)?;
                // dbg!("{environment:?}");
                // dbg!("{:?}", environment.enclosing);
                Ok(eval_expr)
            }
            None => todo!(),
        },
    }
}
