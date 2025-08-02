use insta::assert_yaml_snapshot;
use lexer::{Lexer, Token, TokenType};
use miette::Error;
use std::fmt::Write;

#[test]
fn test_identifiers() {
    let input = "andy formless fo _ _123 _abc ab123
abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ1234567890_";
    let output = Lexer::new(input)
        .filter_map(Result::ok)
        .filter(|t| t.token_type != TokenType::Eof)
        .map(|x| {
            let s = input[x.span.start..=x.span.end].to_string();
            format!("{x:?} - `{s}`")
        })
        .collect::<Vec<String>>();
    insta::with_settings!({
        description => input,
        omit_expression => true
    }, {
        assert_yaml_snapshot!(output);
    });
}

#[test]
fn test_keywords() {
    let input = "and class else false for fun if nil or return super this true var while print";
    let output = Lexer::new(input)
        .filter_map(Result::ok)
        .filter(|t| t.token_type != TokenType::Eof)
        .map(|x| {
            let s = input[x.span.start..=x.span.end].to_string();
            format!("{x:?} - `{s}`")
        })
        .collect::<Vec<String>>();
    insta::with_settings!({
        description => input,
        omit_expression => true
    }, {
        assert_yaml_snapshot!(output);
    });
}

#[test]
fn test_numbers() {
    let input = "123
123.456
.457
123.
90
523.";
    println!("len: {}", input.len());
    let out = Lexer::new(input)
        .filter_map(Result::ok)
        .filter(|t| t.token_type != TokenType::Eof)
        .map(|x| {
            let s = input[x.span.start..=x.span.end].to_string();
            format!("{x:?} - `{s}`")
        })
        .collect::<Vec<String>>();
    insta::with_settings!({
        description => input,
        omit_expression => true
    }, {
        assert_yaml_snapshot!(out);
    });
}

#[test]
fn test_punctuators() {
    let input = "(){};,+-*!===<=>=!=<>/.=!";
    let out = Lexer::new(input)
        .filter_map(Result::ok)
        .filter(|t| t.token_type != TokenType::Eof)
        .map(|x| {
            dbg!(&x);
            let s = input[x.span.start..=x.span.end].to_string();
            format!("{x:?} - `{s}`")
        })
        .collect::<Vec<String>>();
    insta::with_settings!({
        description => input,
        omit_expression => true
    }, {
        assert_yaml_snapshot!(out);
    });
}

#[test]
fn test_strings() {
    let input = "\"\"
\"string\"";
    let lexer = Lexer::new(input);
    let out = lexer
        .filter_map(Result::ok)
        .filter(|t| t.token_type != TokenType::Eof)
        .map(|x| {
            let s = input[x.span.start..=x.span.end].to_string();
            format!("{x:?} - `{s}`")
        })
        .collect::<Vec<String>>();
    insta::with_settings!({
        description => input,
        omit_expression => true
    }, {
        assert_yaml_snapshot!(out);
    });
}

#[test]
fn test_whitespace() {
    let input = "space    tabs				newlines

//

end//";
    let out = Lexer::new(input)
        .filter_map(Result::ok)
        .filter(|t| t.token_type != TokenType::Eof)
        .map(|x| {
            let s = input[x.span.start..=x.span.end].to_string();
            format!("{x:?} - {s}")
        })
        .collect::<Vec<String>>();

    insta::with_settings!({
        description => input,
        omit_expression => true
    }, {
        assert_yaml_snapshot!(out);
    });
}

#[test]
fn test_errors() {
    let out = Lexer::new("#\"//").collect::<Vec<Result<Token, Error>>>();
    assert_eq!(out.len(), 3);
    assert!(out[0].is_err());
    assert!(out[1].is_err());
    assert!(out[2].is_ok());
    let e = out[0].as_ref().expect_err("").to_string();
    assert_eq!(e, "[line 1] Error: Unexpected character: #");
    let e_msg = out[1].as_ref().expect_err("").to_string();
    assert_eq!(e_msg, "[line 1] Error: Unterminated string.");
}

#[test]
fn test_group_literal() {
    let input = "((true))";
    let out = Lexer::new(input)
        .filter_map(Result::ok)
        .filter(|t| t.token_type != TokenType::Eof)
        .map(|x| {
            let s = input[x.span.start..=x.span.end].to_string();
            format!("{x:?} - `{s}`")
        })
        .collect::<Vec<String>>();

    insta::with_settings!({
        description => input,
        omit_expression => true
    }, {
        assert_yaml_snapshot!(out);
    });
}

#[test]
fn test_empty_handling() {
    let input = "";
    let out: String = Lexer::new(input)
        .filter_map(Result::ok)
        .fold(String::new(), |mut out, t| {
            let s = input[t.span.start..t.span.end].to_string();
            let _ = write!(out, "{t:?} - `{s}`");
            out
        });
    assert_yaml_snapshot!(out);
}
