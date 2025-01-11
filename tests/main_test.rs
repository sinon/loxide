use assert_cmd::Command;
use rstest::rstest;

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

#[test]
fn test_evaluate_no_file() {
    Command::cargo_bin("loxide")
        .unwrap()
        .arg("evaluate")
        .arg("text.lox")
        .assert()
        .failure()
        .code(1);
}

#[test]
fn test_evaluate_with_file() {
    Command::cargo_bin("loxide")
        .unwrap()
        .arg("evaluate")
        .arg("tests/fixtures/evaluate.lox")
        .assert()
        .success()
        .code(0)
        .stdout("false\ntrue\nnil\n");
}

#[test]
fn test_evaluate_with_file_error() {
    Command::cargo_bin("loxide")
        .unwrap()
        .arg("evaluate")
        .arg("tests/fixtures/evaluate_error.lox")
        .assert()
        .failure()
        .code(70)
        .stderr(
            "Operand must be a number.
[line 1]\n",
        );
}

#[test]
fn test_run_no_file() {
    Command::cargo_bin("loxide")
        .unwrap()
        .arg("run")
        .arg("text.lox")
        .assert()
        .failure()
        .code(1);
}

#[test]
fn test_run_with_file() {
    Command::cargo_bin("loxide")
        .unwrap()
        .arg("run")
        .arg("tests/fixtures/run.lox")
        .assert()
        .success()
        .code(0)
        .stdout(
            "true
true
true
the expression below is invalid\n3\nsomething\ntrue\ntrue\ntrue\nfalse\nnil\nnil\n98\n98\nbefore\nafter\nafter\nbefore
inner world\nouter baz\nglobal quz\nouter world\nouter baz\nglobal quz\nglobal world\nglobal baz\nglobal quz\n",
        );
}

#[rstest]
#[case("run_error", "Operand must be a number.\n[line 1]\n", "")]
#[case(
    "run_error_undefined_var",
    "Undefined variable 'quz'.\n[line 17]\n",
    "modified foo\ninner quz\nmodified foo\nouter quz\n"
)]
fn test_run_with_file_error(
    #[case] file_name: &str,
    #[case] expected_err: &str,
    #[case] expected_stdout: &str,
) {
    let err = expected_err.to_string();
    let out = expected_stdout.to_string();
    let path = format!("tests/fixtures/{file_name}.lox");
    Command::cargo_bin("loxide")
        .unwrap()
        .arg("run")
        .arg(path)
        .assert()
        .failure()
        .code(70)
        .stderr(err)
        .stdout(out);
}
