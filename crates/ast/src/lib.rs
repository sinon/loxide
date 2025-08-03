use std::fmt::Display;

use lexer::Token;

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
    /// builtin fn
    NativeFunction(NativeFunction),
    /// fn
    LoxFunction { name: String, func_id: u64 },
}

impl From<EvaluatedValue> for bool {
    fn from(val: EvaluatedValue) -> Self {
        match val {
            EvaluatedValue::String(_)
            | EvaluatedValue::Number(_)
            | EvaluatedValue::NativeFunction(_)
            | EvaluatedValue::LoxFunction { .. } => true,
            EvaluatedValue::Nil => false,
            EvaluatedValue::Bool(b) => b,
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
            Self::NativeFunction(native_fn) => write!(f, "{native_fn:?}"),
            Self::LoxFunction { name, func_id } => write!(f, "{name:?}-{func_id:?}"),
        }
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

/// `NativeFunction` is used to represent builtin native functions
#[derive(Clone, Debug)]
pub struct NativeFunction {
    /// `name` of the native function
    pub name: String,
    /// Numbers of arguments that should be passed to `callable`
    pub arity: u8,
    /// A function to be run
    pub callable: fn(&[EvaluatedValue]) -> Result<EvaluatedValue, String>,
}

impl NativeFunction {
    #[must_use]
    pub const fn arity(&self) -> u8 {
        self.arity
    }
    #[allow(clippy::missing_errors_doc)]
    pub fn call(&self, args: &[EvaluatedValue]) -> Result<EvaluatedValue, String> {
        (self.callable)(args)
    }
}

#[derive(Debug, Clone)]
/// `Stmt` represents the possible statements supported
pub enum Stmt<'de> {
    /// A print statement
    Print(Expr<'de>),
    /// An expression statement
    ExpressionStatement(Expr<'de>),
    /// Var statement
    // var <identifier> = expr;
    Var(&'de str, Option<Expr<'de>>),
    /// Block
    Block(Vec<Stmt<'de>>),
    /// If statement
    If(Expr<'de>, Box<Stmt<'de>>, Option<Box<Stmt<'de>>>),
    /// While statement
    While {
        /// The condition that must be `true` for the body to be run
        condition: Expr<'de>,
        /// The statements that will be executed repreatedly if `condition`
        body: Box<Stmt<'de>>,
    },
    /// Func statement
    Function {
        name: Token<'de>,
        parameters: Vec<Token<'de>>,
        body: Vec<Stmt<'de>>,
    },
}

#[derive(Debug, Clone)]
/// `Expr` represents a unit of an AST
pub enum Expr<'de> {
    /// `Binary` is a binary expression such as `1 * 2`
    Binary {
        /// The left item `Expr` in an expression
        left: Box<Expr<'de>>,
        /// The operator to be applied on the `left` and `right` `Expr`
        operator: Token<'de>,
        /// The right item `Expr` in an expression.
        right: Box<Expr<'de>>,
    },
    /// `Unary` is a unary expression such as `!true`
    Unary {
        /// The operator to be applied on the `right` `Expr`
        operator: Token<'de>,
        /// The expression the unary operator will be applied to
        right: Box<Expr<'de>>,
    },
    /// `Literal` is a value
    Literal(LiteralAtom<'de>),
    /// `Grouping` holds other `Expr` such as `(1 * 2)`
    Grouping(Box<Expr<'de>>),
    /// `Variable`
    Variable(Token<'de>),
    /// `Assign`
    Assign(&'de str, Box<Expr<'de>>),
    /// `Logical` - `or` and `and`
    Logical {
        /// The left expression of a Logical expression
        left: Box<Expr<'de>>,
        /// The operator of a Logical expression
        operator: Token<'de>,
        /// The right expression of a Logical expression
        right: Box<Expr<'de>>,
    },
    /// Function `Call`
    Call {
        /// function to be called
        callee: Box<Expr<'de>>,
        /// paren token
        paren: Token<'de>,
        /// arguments to be passed to function call
        arguments: Vec<Expr<'de>>,
    },
}

#[derive(Debug, Clone, PartialEq)]
/// `LiteralAtom` represents the types of literals supported by Lox
pub enum LiteralAtom<'de> {
    /// `String` literal for example `"foo"`
    String(&'de str),
    /// Number literal for example `123.1`
    Number(f64),
    /// Nil literal
    Nil,
    /// Bool literals `false` or `true`
    Bool(bool),
}
