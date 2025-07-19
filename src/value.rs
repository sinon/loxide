use std::fmt::{Display, write};

use crate::interpreter::NativeFunction;

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
    LoxFunction {
        name: String,
        binding: Option<Box<EvaluatedValue>>,
    },
}

impl EvaluatedValue {
    pub(crate) const fn is_truthy(&self) -> bool {
        match self {
            Self::String(_) | Self::Number(_) => true,
            Self::Nil => false,
            Self::Bool(b) => *b,
            Self::NativeFunction(_f) => true,
            Self::LoxFunction { name, binding } => true,
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
            EvaluatedValue::LoxFunction { name, binding } => true,
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
            Self::LoxFunction { name, binding } => write!(f, "{name:?}"),
        }
    }
}
