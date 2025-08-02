//! Interpreter module
//!
//! Responsible for running the AST and returning the computed values
//!

use std::{cell::RefCell, collections::HashMap, fmt, rc::Rc};

use crate::{builtins, value::EvaluatedValue};
use lexer::{Token, TokenType};
use parser::{Expr, LiteralAtom, Parser, Stmt};

/// `NativeFunction` is used to represent builtin native functions
#[derive(Clone)]
pub struct NativeFunction {
    /// `name` of the native function
    pub name: String,
    /// Numbers of arguments that should be passed to `callable`
    pub arity: u8,
    /// A function to be run
    pub callable: fn(&mut Interpreter, &[EvaluatedValue]) -> Result<EvaluatedValue, String>,
}

impl fmt::Debug for NativeFunction {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "NativeFunction({})", self.name)
    }
}

impl NativeFunction {
    const fn arity(&self, _interpreter: &Interpreter) -> u8 {
        self.arity
    }
    fn call(
        &self,
        interpreter: &mut Interpreter,
        args: &[EvaluatedValue],
    ) -> Result<EvaluatedValue, String> {
        (self.callable)(interpreter, args)
    }
}

/// A user defined lox function
#[derive(Debug, Clone)]
pub struct LoxFunction<'de> {
    /// The identifier
    pub name: Token<'de>,
    /// The parameter values
    pub parameters: Vec<Token<'de>>,
    /// The body of the function
    pub body: Vec<Stmt<'de>>,
}

impl<'de> LoxFunction<'de> {
    #[allow(dead_code)]
    fn arity(&self) -> u8 {
        u8::try_from(self.parameters.len()).expect("arity < 255 is enforced by parser")
    }
    #[allow(dead_code)]
    fn call(
        &self,
        interpreter: &mut Interpreter<'de>,
        args: &[EvaluatedValue],
    ) -> Result<EvaluatedValue, String> {
        let _args_env: HashMap<_, _> = self
            .parameters
            .iter()
            .zip(args.iter())
            .map(|(param, arg)| (param.origin, (Some(arg.clone()),)))
            .collect();
        let saved_env = interpreter.environment.clone();
        let block = Stmt::Block(self.body.clone());
        evaluate_statement(&block, interpreter)?;
        interpreter.environment = saved_env;
        todo!("Implement the `call` fn for `LoxFunction`")
    }
}

/// `Interpreter`
/// responsible for iterating over the rusults of parser
/// and evaluating the statements and expressions encountered
pub struct Interpreter<'de> {
    parser: Parser<'de>,
    environment: Environment<'de>,
    globals: Environment<'de>,
    lox_functions: HashMap<u64, LoxFunction<'de>>,
    counter: u64,
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
            None => {
                if let Some(parent) = &self.enclosing {
                    let mut p = parent.borrow_mut();
                    p.assign(key, value)?;
                    Ok(())
                } else {
                    self.data.insert(key, value.clone());
                    Ok(())
                }
            }
        }
    }
    fn var_assign(&mut self, key: &'de str, value: &EvaluatedValue) {
        // var <identifier> = expr;
        // Only updates the current environment scope
        self.data.insert(key, value.clone());
    }
}

impl<'de> Interpreter<'de> {
    /// Create a new `Interpreter` to process a given input source code
    /// # Panics
    /// Will panic if `SystemTime::now()` returns value before `UNIX_EPOCH`
    #[must_use]
    pub fn new(input: &'de str) -> Self {
        let mut global_data = HashMap::new();
        global_data.insert(
            "clock",
            EvaluatedValue::NativeFunction(NativeFunction {
                name: "clock".to_string(),
                arity: 0,
                callable: builtins::clock,
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
            lox_functions: HashMap::new(),
            counter: 0,
        }
    }

    pub(crate) fn get_lox_fn(&self, id: u64) -> &LoxFunction {
        self.lox_functions.get(&id).map_or_else(
            || panic!("# TODO Improve error handling for `get_lox_fn`",),
            |func| func,
        )
    }
    const fn alloc_id(&mut self) -> u64 {
        self.counter += 1;
        self.counter
    }
}

impl Iterator for Interpreter<'_> {
    type Item = Result<(), u8>;

    fn next(&mut self) -> Option<Self::Item> {
        let stmt = self.parser.next()?;
        stmt.map_or(Some(Err(65)), |s| {
            let eval_stmt = evaluate_statement(&s, self);
            match eval_stmt {
                Ok(()) => Some(Ok(())),
                Err(_) => Some(Err(70)),
            }
        })
    }
}

fn evaluate_statement<'de>(
    stmt: &Stmt<'de>,
    interpreter: &mut Interpreter<'de>,
) -> Result<(), String> {
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
        Stmt::Function {
            name,
            parameters,
            body,
        } => {
            let func_id = interpreter.alloc_id();
            let lox_fun = LoxFunction {
                name: name.clone(),
                parameters: parameters.clone(),
                body: body.clone(),
            };
            interpreter.lox_functions.insert(func_id, lox_fun);
            interpreter.environment.assign(
                name.origin,
                &EvaluatedValue::LoxFunction {
                    name: name.origin.to_string(),
                    func_id,
                },
            )?;
            // interpreter.globals.data.insert(&name.to_string(), lox_fun);
        }
    }
    Ok(())
}

fn evaluate_expression<'de>(
    expr: &Expr<'de>,
    interpreter: &mut Interpreter<'de>,
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
                        _ => {
                            panic!("{op:?} is not a valid token type for Expr::Binary with Numbers")
                        }
                    }
                }
                (EvaluatedValue::String(s1), EvaluatedValue::String(s2), operator) => {
                    match operator.token_type {
                        TokenType::Plus => Ok(EvaluatedValue::String(s1 + &s2)),
                        TokenType::EqualEqual => Ok(EvaluatedValue::Bool(s1 == s2)),
                        TokenType::BangEqual => Ok(EvaluatedValue::Bool(s1 != s2)),
                        // TODO: Make unrepresentable by narrowing `operator` to `BinaryOperator:Not|Negate`
                        _ => panic!(
                            "{operator:?} is not a valid token type for Expr:Binary with Strings"
                        ),
                    }
                }
                (EvaluatedValue::String(_), EvaluatedValue::Number(_), operator)
                | (EvaluatedValue::Number(_), EvaluatedValue::String(_), operator) => {
                    match operator.token_type {
                        TokenType::EqualEqual => Ok(EvaluatedValue::Bool(false)),
                        TokenType::BangEqual => Ok(EvaluatedValue::Bool(true)),
                        _ => panic!("{operator:?} is not supported for String<>Number"),
                    }
                }
                (EvaluatedValue::Bool(b1), EvaluatedValue::Bool(b2), operator) => {
                    match operator.token_type {
                        TokenType::BangEqual => Ok(EvaluatedValue::Bool(b1 != b2)),
                        TokenType::EqualEqual => Ok(EvaluatedValue::Bool(b1 == b2)),
                        _ => panic!("{operator:?} is not for suppoer Bool / Bool binary"),
                    }
                }
                (l, r, op) => todo!("Add handling for {l} {r} {op:?}"),
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
                        EvaluatedValue::NativeFunction(_f) => todo!(),
                        EvaluatedValue::LoxFunction {
                            name: _,
                            func_id: _,
                        } => todo!(),
                    },
                ),
                TokenType::Minus => r.as_ref().map_or_else(
                    |_| todo!(),
                    |v| match v {
                        EvaluatedValue::String(_) => todo!(),
                        EvaluatedValue::Number(n) => Ok(EvaluatedValue::Number(-n)),
                        EvaluatedValue::Nil => todo!(),
                        EvaluatedValue::Bool(_) => todo!(),
                        EvaluatedValue::NativeFunction(_f) => todo!(),
                        EvaluatedValue::LoxFunction {
                            name: _,
                            func_id: _,
                        } => todo!(),
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
                    eprintln!("Undefined variable `{}`.", token.origin);
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
            paren: _,
            arguments,
        } => {
            let callee_fn = evaluate_expression(callee, interpreter)?;
            let mut args: Vec<EvaluatedValue> = Vec::new();
            for arg in arguments {
                args.push(evaluate_expression(arg, interpreter)?);
            }

            match &callee_fn {
                EvaluatedValue::NativeFunction(f) => {
                    if f.arity(interpreter) as usize != args.len() {
                        eprintln!("Expected {} arguments but got {}.", f.arity, args.len());
                        return Err("Incorrect arity".to_string());
                    }
                    f.call(interpreter, &args)
                }
                EvaluatedValue::LoxFunction { func_id, .. } => {
                    let _ = interpreter.get_lox_fn(*func_id);
                    todo!("Fix lifetime issue with f.call")
                    // f.call(interpreter, &args)
                }
                _ => Err("not callable".to_string()),
            }
        }
    }
}
