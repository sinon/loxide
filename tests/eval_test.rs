use insta::{assert_debug_snapshot, assert_snapshot};
use loxide::eval::Eval;
use rstest::*;

macro_rules! set_snapshot_suffix {
    ($($expr:expr),*) => {
        let mut settings = insta::Settings::clone_current();
        settings.set_snapshot_suffix(format!($($expr,)*));
        settings.set_description(format!($($expr,)*));
        settings.set_omit_expression(true);
        let _guard = settings.bind_to_scope();
    }
}

#[rstest]
#[case("true")]
#[case("false")]
#[case("nil")]
#[case("\"hello world\"")]
#[case("10.4")]
#[case("10")]
#[case("10.0")]
#[case("(\"hello world!\")")]
#[case("(10.5)")]
#[case("((false))")]
#[case("-73")]
#[case("!true")]
#[case("!nil")]
#[case("!10.40")]
#[case("!((false))")]
#[case("42 / 5")]
#[case("18 * 3 / (3 * 6)")]
#[case("(10.40 * 2) / 2")]
#[case("23 + 28 - (-(61 - 99))")]
#[case("\"foo\" + \"bar\"")]
#[case("\"42\" + \"24\"")]
#[case("57 > -65")]
#[case("11 >= 11")]
#[case("(54 - 67) >= -(114 / 57 + 11)")]
#[case("\"hello\" == \"world\"")]
#[case("\"foo\" != \"bar\"")]
#[case("\"foo\" == \"foo\"")]
#[case("61 == \"61\"")]
#[case("61 != \"61\"")]
#[case("\"61\" != 61")]
fn test_eval_literals(#[case] input: &str) {
    set_snapshot_suffix!("{}", input);
    let exprs: Vec<String> = Eval::new(input)
        .take_while(|x| x.is_ok())
        .map(|e| match e {
            Ok(exp) => format!("{}", exp),
            Err(_) => "".to_string(),
        })
        .collect();
    assert_snapshot!(exprs.join("\n"));
}

#[rstest]
#[case("-\"hello world!\"")]
#[case("-true")]
#[case("-(\"world\" + \"foo\")")]
#[case("\"foo\" * 42")]
#[case("true / 2")]
#[case("(\"foo\" * \"bar\")")]
#[case("false / true")]
#[case("\"foo\" + true")]
#[case("42 - true")]
#[case("true + false")]
#[case("\"foo\" - \"bar\"")]
#[case("\"foo\" < false")]
#[case("true < 2")]
#[case("(\"foo\" + \"bar\") < 42")]
#[case("false > true")]
fn test_eval_errors(#[case] input: &str) {
    set_snapshot_suffix!("{}", input);
    let mut errors = Vec::new();
    for e in Eval::new(input) {
        match e {
            Ok(_) => {}
            Err(e) => errors.push(e),
        }
    }
    assert_debug_snapshot!(errors);
}
