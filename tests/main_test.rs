use assert_cmd::Command;

#[test]
fn test_cli_version() {
    Command::cargo_bin("loxide")
        .unwrap()
        .arg("--version")
        .assert()
        .success()
        .code(0)
        .stdout("loxide 0.1.0\n".to_string())
        .stderr("");

    Command::cargo_bin("loxide")
        .unwrap()
        .arg("-V")
        .assert()
        .success()
        .code(0)
        .stdout("loxide 0.1.0\n".to_string())
        .stderr("");
}
#[test]
fn test_tokenize_no_file() {
    Command::cargo_bin("loxide")
        .unwrap()
        .arg("tokenize")
        .arg("text.lox")
        .assert()
        .failure()
        .code(1);
}

#[test]
fn test_tokenize_with_file() {
    Command::cargo_bin("loxide")
        .unwrap()
        .arg("tokenize")
        .arg("tests/fixtures/lexer.lox")
        .assert()
        .success()
        .code(0)
        .stdout(
            "STRING \"hello\" hello\nNUMBER 123 123.0\nFALSE false null\nNIL nil null\nEOF  null\n",
        );
}

#[test]
fn test_tokenize_with_file_error() {
    Command::cargo_bin("loxide")
        .unwrap()
        .arg("tokenize")
        .arg("tests/fixtures/lexer_error.lox")
        .assert()
        .failure()
        .code(65)
        .stderr("[line 1] Error: Unexpected character: #\n[line 1] Error: Unterminated string.\n")
        .stdout("EOF  null\n");
}

#[test]
fn test_parse_no_file() {
    Command::cargo_bin("loxide")
        .unwrap()
        .arg("parse")
        .arg("text.lox")
        .assert()
        .failure()
        .code(1);
}

#[test]
fn test_parse_with_file() {
    Command::cargo_bin("loxide")
        .unwrap()
        .arg("parse")
        .arg("tests/fixtures/parser.lox")
        .assert()
        .success()
        .code(0)
        .stdout("(- (+ 52.0 80.0) 94.0)\n(> (< 83.0 99.0) 115.0)\n");
}

#[test]
fn test_parse_with_file_error() {
    Command::cargo_bin("loxide")
        .unwrap()
        .arg("parse")
        .arg("tests/fixtures/parser_error.lox")
        .assert()
        .failure()
        .code(65)
        .stderr("[line 1] Error at \')\': Expect expression.\n")
        .stdout("");
}
