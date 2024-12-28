use loxide::lexer::{Lexer, Token};
use miette::Error;

#[test]
fn test_identifiers() {
    let input = "andy formless fo _ _123 _abc ab123
abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ1234567890_";
    let lex = Lexer::new(input);

    let output: Vec<String> = lex
        .into_iter()
        .filter_map(Result::ok)
        .map(|x| format!("{:}", x))
        .collect();
    let output = output.join("\n");
    assert_eq!(
        output,
        "IDENTIFIER andy null
IDENTIFIER formless null
IDENTIFIER fo null
IDENTIFIER _ null
IDENTIFIER _123 null
IDENTIFIER _abc null
IDENTIFIER ab123 null
IDENTIFIER abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ1234567890_ null"
    );
}

#[test]
fn test_keywords() {
    let input = "and class else false for fun if nil or return super this true var while print";
    let output = Lexer::new(input)
        .into_iter()
        .filter_map(Result::ok)
        .map(|x| format!("{}", x))
        .collect::<Vec<String>>()
        .join("\n");
    assert_eq!(
        output,
        "AND and null
CLASS class null
ELSE else null
FALSE false null
FOR for null
FUN fun null
IF if null
NIL nil null
OR or null
RETURN return null
SUPER super null
THIS this null
TRUE true null
VAR var null
WHILE while null
PRINT print null"
    )
}

#[test]
fn test_numbers() {
    let out = Lexer::new(
        "123
123.456
.457
123.
90",
    )
    .into_iter()
    .filter_map(Result::ok)
    .map(|x| format!("{}", x))
    .collect::<Vec<String>>()
    .join("\n");
    assert_eq!(
        out,
        "NUMBER 123 123.0
NUMBER 123.456 123.456
DOT . null
NUMBER 457 457.0
NUMBER 123 123.0
DOT . null
NUMBER 90 90.0"
    )
}

#[test]
fn test_punctuators() {
    let out = Lexer::new("(){};,+-*!===<=>=!=<>/.=!")
        .into_iter()
        .filter_map(Result::ok)
        .map(|x| format!("{}", x))
        .collect::<Vec<String>>()
        .join("\n");
    assert_eq!(
        out,
        "LEFT_PAREN ( null
RIGHT_PAREN ) null
LEFT_BRACE { null
RIGHT_BRACE } null
SEMICOLON ; null
COMMA , null
PLUS + null
MINUS - null
STAR * null
BANG_EQUAL != null
EQUAL_EQUAL == null
LESS_EQUAL <= null
GREATER_EQUAL >= null
BANG_EQUAL != null
LESS < null
GREATER > null
SLASH / null
DOT . null
EQUAL = null
BANG ! null"
    );
}

#[test]
fn test_strings() {
    let out = Lexer::new(
        "\"\"
\"string\"",
    )
    .into_iter()
    .filter_map(Result::ok)
    .map(|x| format!("{x}"))
    .collect::<Vec<String>>()
    .join("\n");
    assert_eq!(
        out,
        "STRING \"\" 
STRING \"string\" string"
    );
}

#[test]
fn test_whitespace() {
    let out = Lexer::new(
        "space    tabs				newlines

//


end//",
    )
    .into_iter()
    .filter_map(Result::ok)
    .map(|x| format!("{x}"))
    .collect::<Vec<String>>()
    .join("\n");

    assert_eq!(
        out,
        "IDENTIFIER space null
IDENTIFIER tabs null
IDENTIFIER newlines null
IDENTIFIER end null"
    );
}

#[test]
fn test_errors() {
    let out = Lexer::new("#\"//")
        .into_iter()
        .collect::<Vec<Result<Token, Error>>>();
    assert_eq!(out.len(), 2);
    assert_eq!(out[0].is_err(), true);
    assert_eq!(out[1].is_err(), true);
    let e = out[0].as_ref().err().expect("").to_string();
    assert_eq!(e, "[line 1] Error: Unexpected character: #");
    let e_msg = out[1].as_ref().err().expect("").to_string();
    assert_eq!(e_msg, "[line 1] Error: Unterminated string.");
}
