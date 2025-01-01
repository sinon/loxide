use insta::assert_snapshot;
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
