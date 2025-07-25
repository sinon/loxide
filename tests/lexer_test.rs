use insta::assert_yaml_snapshot;
use loxide::lexer::{Lexer, Token};
use miette::Error;
use std::fmt::Write;

#[test]
fn test_identifiers() {
    let input = "andy formless fo _ _123 _abc ab123
abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ1234567890_";
    let output = Lexer::new(input)
        .filter_map(Result::ok)
        .map(|x| format!("{x:}"))
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
        .map(|x| format!("{x}"))
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
    let out = Lexer::new(input)
        .filter_map(Result::ok)
        .map(|x| format!("{x}"))
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
        .map(|x| format!("{x}"))
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
    let out = Lexer::new(input)
        .filter_map(Result::ok)
        .map(|x| format!("{x}"))
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
        .map(|x| format!("{x}"))
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
    let out = Lexer::new("((true))")
        .filter_map(Result::ok)
        .map(|x| format!("{x}"))
        .collect::<Vec<String>>()
        .join("\n");
    assert_eq!(
        out,
        "LEFT_PAREN ( null\nLEFT_PAREN ( null\nTRUE true null\nRIGHT_PAREN ) null\nRIGHT_PAREN ) null\nEOF  null"
    );
}

#[test]
fn test_empty_handling() {
    let out: String = Lexer::new("")
        .filter_map(Result::ok)
        .fold(String::new(), |mut out, t| {
            let _ = write!(out, "{t}");
            out
        });
    assert_yaml_snapshot!(out);
}
