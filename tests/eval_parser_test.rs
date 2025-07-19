use insta::assert_snapshot;
use loxide::eval_parser::Parser;
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
fn test_parser_literals(#[case] input: &str) {
    set_snapshot_suffix!("{}", input);
    let exprs: Vec<String> = Parser::new(input)
        .take_while(std::result::Result::is_ok)
        .map(|e| match e {
            Ok(exp) => format!("{exp}"),
            Err(_) => String::new(),
        })
        .collect();
    assert_snapshot!(exprs.join("\n"));
}

#[rstest]
#[case("(\"foo\")")]
#[case("(1)")]
#[case("((true))")]
fn test_parser_parentheses(#[case] input: &str) {
    set_snapshot_suffix!("{}", input);
    let exprs: Vec<String> = Parser::new(input)
        .take_while(std::result::Result::is_ok)
        .map(|e| match e {
            Ok(exp) => format!("{exp}"),
            Err(_) => String::new(),
        })
        .collect();
    assert_snapshot!(exprs.join("\n"));
}

#[rstest]
#[case("!true")]
#[case("!false")]
#[case("-10")]
fn test_parser_unary(#[case] input: &str) {
    set_snapshot_suffix!("{}", input);
    let exprs: Vec<String> = Parser::new(input)
        .take_while(std::result::Result::is_ok)
        .map(|e| match e {
            Ok(exp) => format!("{exp}"),
            Err(_) => String::new(),
        })
        .collect();
    insta::with_settings!({
        description => input, // the template source code
        omit_expression => true // do not include the default expression
    }, {
        assert_snapshot!(exprs.join("\n"));
    });
}

#[rstest]
#[case("16 * 38 / 58")]
#[case("52 + 80 - 94")]
fn test_parser_arimethic(#[case] input: &str) {
    set_snapshot_suffix!("{}", input);
    let exprs: Vec<String> = Parser::new(input)
        .take_while(std::result::Result::is_ok)
        .map(|e| match e {
            Ok(exp) => format!("{exp}"),
            Err(_) => String::new(),
        })
        .collect();
    assert_snapshot!(exprs.join("\n"));
}

#[rstest]
#[case("83 < 99 > 115")]
#[case("52 <= 80 >= 94")]
fn test_parser_comparison(#[case] input: &str) {
    set_snapshot_suffix!("{}", input);
    let exprs: Vec<String> = Parser::new(input)
        .take_while(std::result::Result::is_ok)
        .map(|e| match e {
            Ok(exp) => format!("{exp}"),
            Err(_) => String::new(),
        })
        .collect();

    insta::with_settings!({
        description => input, // the template source code
        omit_expression => true // do not include the default expression
    }, {
        assert_snapshot!(exprs.join("\n"));
    });
}

#[rstest]
#[case("\"baz\" == \"baz\"")]
#[case("10 != 9")]
fn test_parser_equality(#[case] input: &str) {
    set_snapshot_suffix!("{}", input);
    let exprs: Vec<String> = Parser::new(input)
        .take_while(std::result::Result::is_ok)
        .map(|e| match e {
            Ok(exp) => format!("{exp}"),
            Err(_) => String::new(),
        })
        .collect();
    assert_snapshot!(exprs.join("\n"));
}

#[rstest]
#[case("(72 +)")]
#[case("\"foo")]
fn test_parser_error(#[case] input: &str) {
    set_snapshot_suffix!("{}", input);
    for expr in Parser::new(input) {
        match expr {
            Ok(_) => todo!(),
            Err(err) => {
                assert_snapshot!(err);
                break;
            }
        }
    }
}
