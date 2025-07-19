use std::time::{SystemTime, UNIX_EPOCH};

use crate::eval::EvaluatedValue;
use crate::interpreter;

pub fn clock(
    _interpreter: &mut interpreter::Interpreter,
    _args: &[EvaluatedValue],
) -> Result<EvaluatedValue, String> {
    let start = SystemTime::now();
    #[allow(clippy::cast_precision_loss)]
    start.duration_since(UNIX_EPOCH).map_or_else(
        |_| Err("Unable to calculate time since UNIX_EPOC".to_string()),
        |since_the_epoch| Ok(EvaluatedValue::Number(since_the_epoch.as_millis() as f64)),
    )
}
