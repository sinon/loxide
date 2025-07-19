//! Run module
//!
//! Responsible for running the AST and returning the computed values
//!

use std::{
    cell::RefCell,
    collections::HashMap,
    fmt,
    rc::Rc,
    time::{SystemTime, UNIX_EPOCH},
};

use crate::{
    eval::EvaluatedValue,
    lexer::TokenType,
    parser::{Expr, LiteralAtom, Parser, Stmt},
};

trait Callable {
    fn arity(&self, interpreter: &Run) -> u8;
    fn call(
        &self,
        interpreter: &mut Run,
        args: &[EvaluatedValue],
    ) -> Result<EvaluatedValue, String>;
}

#[derive(Clone)]
pub struct NativeFunction {
    pub name: String,
    pub arity: u8,
    pub callable: fn(&mut Run, &[EvaluatedValue]) -> Result<EvaluatedValue, String>,
}

impl fmt::Debug for NativeFunction {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "NativeFunction({})", self.name)
    }
}

impl Callable for NativeFunction {
    fn arity(&self, _interpreter: &Run) -> u8 {
        self.arity
    }
    fn call(
        &self,
        interpreter: &mut Run,
        args: &[EvaluatedValue],
    ) -> Result<EvaluatedValue, String> {
        (self.callable)(interpreter, args)
    }
}

/// `Run`
/// an iterator that consumes expressions from the parser and tries to evaluate them.
pub struct Run<'de> {
    parser: Parser<'de>,
    environment: Environment<'de>,
    globals: Environment<'de>,
}

#[derive(Debug, Clone)]
struct Environment<'de> {
    data: HashMap<&'de str, EvaluatedValue>,
    enclosing: Option<Rc<RefCell<Environment<'de>>>>,
}

impl<'de> Environment<'de> {
    fn new() -> Self {
        Environment {
            data: HashMap::new(),
            enclosing: None,
        }
    }

    fn from_parent(enclosing: Self) -> Self {
        Environment {
            data: HashMap::new(),
            enclosing: Some(Rc::new(RefCell::new(enclosing))),
        }
    }

    fn get(&self, key: &'de str) -> Option<EvaluatedValue> {
        // get the given key from the environment
        // checks the current scope level, then works up through the enclosing env
        self.data.get(key).map_or_else(
            || {
                self.enclosing
                    .as_ref()
                    .and_then(|parent| parent.borrow().get(key))
            },
            |v| Some(v.clone()),
        )
    }

    fn assign(&mut self, key: &'de str, value: &EvaluatedValue) -> Result<(), String> {
        match self.data.get(key) {
            Some(_) => {
                self.data.insert(key, value.clone());
                Ok(())
            }
            None => match &self.enclosing {
                Some(parent) => {
                    let mut p = parent.borrow_mut();
                    p.assign(key, value)?;
                    Ok(())
                }
                None => todo!(),
            },
        }
    }
    fn var_assign(&mut self, key: &'de str, value: &EvaluatedValue) {
        // var <identifier> = expr;
        // Only updates the current environment scope
        self.data.insert(key, value.clone());
    }
}

impl<'de> Run<'de> {
    /// Create a new `Interpreter` to process a given input source code
    #[must_use]
    pub fn new(input: &'de str) -> Self {
        let mut global_data = HashMap::new();
        global_data.insert(
            "clock",
            EvaluatedValue::NativeFunction(NativeFunction {
                name: "clock".to_string(),
                arity: 0,
                callable: |_, _| {
                    let start = SystemTime::now();
                    let since_the_epoch = start.duration_since(UNIX_EPOCH).unwrap();

                    Ok(EvaluatedValue::Number(since_the_epoch.as_millis() as f64))
                },
            }),
        );
        let globals = Environment {
            data: global_data,
            enclosing: None,
        };
        Self {
            parser: Parser::new(input),
            environment: Environment::new(),
            globals,
        }
    }
}

impl Iterator for Run<'_> {
    type Item = Result<(), u8>;

    fn next(&mut self) -> Option<Self::Item> {
        let stmt = self.parser.next()?;
        match stmt {
            Ok(s) => {
                let eval_stmt = evaluate_statement(&s, self);
                match eval_stmt {
                    Ok(()) => Some(Ok(())),
                    Err(_) => Some(Err(70)),
                }
            }
            Err(_) => Some(Err(65)),
        }
    }
}

fn evaluate_statement<'de>(stmt: &Stmt<'de>, interpreter: &mut Run<'de>) -> Result<(), String> {
    match stmt {
        Stmt::Print(expr) => {
            let val = evaluate_expression(expr, interpreter)?;
            println!("{val}");
        }
        Stmt::If(cond, then_branch, else_branch) => {
            let eval_cond = evaluate_expression(cond, interpreter)?;
            if eval_cond.into() {
                evaluate_statement(then_branch, interpreter)?;
            } else if let Some(e) = else_branch {
                evaluate_statement(e, interpreter)?;
            }
        }
        Stmt::ExpressionStatement(expr) => {
            evaluate_expression(expr, interpreter)?;
        }
        Stmt::Var(name, expr) => match expr {
            Some(v) => {
                let evalutated_val = evaluate_expression(v, interpreter)?;
                interpreter.environment.var_assign(name, &evalutated_val);
            }
            None => {
                interpreter
                    .environment
                    .var_assign(name, &EvaluatedValue::Nil);
            }
        },
        Stmt::Block(stmts) => {
            let new_env = Environment::from_parent(interpreter.environment.clone());
            interpreter.environment = new_env;
            for stmt in stmts {
                evaluate_statement(stmt, interpreter)?;
            }
            // # TODO: Remove this clone
            interpreter.environment = interpreter
                .environment
                .clone()
                .enclosing
                .expect("`new_env` declared above will always have `enclosing` set")
                .borrow()
                .clone();
        }
        Stmt::While { condition, body } => loop {
            if !(evaluate_expression(condition, interpreter)?.is_truthy()) {
                break;
            }
            evaluate_statement(body, interpreter)?;
        },
    }
    Ok(())
}

fn evaluate_expression<'de>(
    expr: &Expr<'de>,
    interpreter: &mut Run<'de>,
    // environment: &mut Environment<'de>,
    // globals: &mut Environment<'de>,
) -> Result<EvaluatedValue, String> {
    match expr {
        Expr::Binary {
            left,
            operator,
            right,
        } => {
            let l_expr = evaluate_expression(left, interpreter)?;
            let r_expr = evaluate_expression(right, interpreter)?;
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
                (EvaluatedValue::Bool(b1), EvaluatedValue::Bool(b2), operator) => {
                    match operator.token_type {
                        TokenType::BangEqual => Ok(EvaluatedValue::Bool(b1 != b2)),
                        TokenType::EqualEqual => Ok(EvaluatedValue::Bool(b1 == b2)),
                        _ => panic!("{operator} is not for suppoer Bool / Bool binary"),
                    }
                }
                (l, r, op) => todo!("Add handling for {l} {r} {op}"),
            }
        }
        Expr::Unary { operator, right } => {
            let r = evaluate_expression(right, interpreter);
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
                        EvaluatedValue::NativeFunction(f) => todo!(),
                    },
                ),
                TokenType::Minus => r.as_ref().map_or_else(
                    |_| todo!(),
                    |v| match v {
                        EvaluatedValue::String(_) => todo!(),
                        EvaluatedValue::Number(n) => Ok(EvaluatedValue::Number(-n)),
                        EvaluatedValue::Nil => todo!(),
                        EvaluatedValue::Bool(_) => todo!(),
                        EvaluatedValue::NativeFunction(f) => todo!(),
                    },
                ),
                // TODO: Make unrepresentable by narrowing `operator` to `UnaryOperator:Not|Negate`
                _ => {
                    panic!("{:?} is not a valid unary token type", operator.token_type)
                }
            }
        }
        Expr::Literal(literal_atom) => match literal_atom {
            LiteralAtom::String(s) => Ok(EvaluatedValue::String((*s).to_string())),
            LiteralAtom::Number(num) => Ok(EvaluatedValue::Number(*num)),
            LiteralAtom::Nil => Ok(EvaluatedValue::Nil),
            LiteralAtom::Bool(b) => Ok(EvaluatedValue::Bool(*b)),
        },
        Expr::Grouping(expr) => evaluate_expression(expr, interpreter),
        Expr::Variable(token) => match interpreter.environment.get(token.origin) {
            Some(v) => Ok(v),
            None => interpreter.globals.get(token.origin).map_or_else(
                || {
                    eprintln!("Undefined variable '{}'.", token.origin);
                    eprintln!("[line {}]", token.line);
                    Err("Undefined var".to_string())
                },
                Ok,
            ),
        },
        Expr::Assign(name, expr) => {
            if interpreter.environment.get(name).is_some() {
                let eval_expr = evaluate_expression(expr, interpreter)?;
                interpreter.environment.assign(name, &eval_expr)?;
                Ok(eval_expr)
            } else {
                eprintln!("Undefined variable '{name}'.");
                Err("Undefined var".to_string())
            }
        }
        Expr::Logical {
            left,
            operator,
            right,
        } => {
            let left_val = evaluate_expression(left, interpreter)?;
            let left_truth: bool = left_val.is_truthy();
            if operator.token_type == TokenType::Or {
                if left_truth {
                    return Ok(left_val);
                }
            } else if !left_truth {
                return Ok(left_val);
            }
            Ok(evaluate_expression(right, interpreter)?)
        }
        Expr::Call {
            callee,
            paren,
            arguments,
        } => {
            let callee_fn = evaluate_expression(callee, interpreter)?;
            let mut args: Vec<EvaluatedValue> = Vec::new();
            for arg in arguments {
                args.push(evaluate_expression(arg, interpreter)?);
            }
            // TODO: Need to check function arity on `EvaluatedValue` for Fn

            match callee_fn {
                EvaluatedValue::NativeFunction(native_function) => {
                    let fun = native_function.call(interpreter, &args);
                    return fun;
                }
                _ => todo!(),
            }
        }
    }
}
