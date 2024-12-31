use insta::assert_snapshot;
use loxide::lexer::{Lexer, Token};
use loxide::parser::Parser;
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
    let tokens: Vec<Token> = Lexer::new(input).filter_map(Result::ok).collect();
    let exprs: Vec<String> = Parser::new(tokens)
        .take_while(|x| x.is_ok())
        .map(|e| match e {
            Ok(exp) => format!("{}", exp),
            Err(_) => "".to_string(),
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
    let tokens: Vec<Token> = Lexer::new(input).filter_map(Result::ok).collect();
    let exprs: Vec<String> = Parser::new(tokens)
        .take_while(|x| x.is_ok())
        .map(|e| match e {
            Ok(exp) => format!("{}", exp),
            Err(_) => "".to_string(),
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
    let tokens: Vec<Token> = Lexer::new(input).filter_map(Result::ok).collect();
    let exprs: Vec<String> = Parser::new(tokens)
        .take_while(|x| x.is_ok())
        .map(|e| match e {
            Ok(exp) => format!("{}", exp),
            Err(_) => "".to_string(),
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
    let tokens: Vec<Token> = Lexer::new(input).filter_map(Result::ok).collect();
    let exprs: Vec<String> = Parser::new(tokens)
        .take_while(|x| x.is_ok())
        .map(|e| match e {
            Ok(exp) => format!("{}", exp),
            Err(_) => "".to_string(),
        })
        .collect();
    assert_snapshot!(exprs.join("\n"));
}

#[rstest]
#[case("83 < 99 > 115")]
#[case("52 <= 80 >= 94")]
fn test_parser_comparison(#[case] input: &str) {
    set_snapshot_suffix!("{}", input);
    let tokens: Vec<Token> = Lexer::new(input).filter_map(Result::ok).collect();
    let exprs: Vec<String> = Parser::new(tokens)
        .take_while(|x| x.is_ok())
        .map(|e| match e {
            Ok(exp) => format!("{}", exp),
            Err(_) => "".to_string(),
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
    let tokens: Vec<Token> = Lexer::new(input).filter_map(Result::ok).collect();
    let exprs: Vec<String> = Parser::new(tokens)
        .take_while(|x| x.is_ok())
        .map(|e| match e {
            Ok(exp) => format!("{}", exp),
            Err(_) => "".to_string(),
        })
        .collect();
    assert_snapshot!(exprs.join("\n"));
}

#[rstest]
#[case("(72 +)")]
#[case("\"foo")]
fn test_parser_error(#[case] input: &str) {
    set_snapshot_suffix!("{}", input);
    let tokens: Vec<Token> = Lexer::new(&input).filter_map(Result::ok).collect();
    for expr in Parser::new(tokens) {
        match expr {
            Ok(_) => todo!(),
            Err(err) => {
                assert_snapshot!(err);
                break;
            }
        }
    }
    // let exprs: Vec<String> = Parser::new(tokens)
    //     .take_while(|x| !x.is_err())
    //     .map(|e| match e {
    //         Ok(exp) => format!("{}", exp),
    //         Err(err) => panic!("!"),
    //     })
    //     .collect();
    // assert_eq!(
    //     exprs.join("\n"),
    //     "[line 1] Error at ')': Expect expression."
    // )
}
