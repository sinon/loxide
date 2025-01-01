use clap::Subcommand;
use loxide::lexer::Lexer;
use loxide::parser::Parser;
use miette::{IntoDiagnostic, Result, WrapErr};
use std::fs;
use std::path::PathBuf;
use std::process::ExitCode;

use clap::Parser as ClapParser;

#[derive(ClapParser, Debug)]
#[command(version, about, long_about = None)]
struct Args {
    #[command(subcommand)]
    commands: Commands,
}
#[derive(Subcommand, Debug)]
enum Commands {
    Tokenize { filename: PathBuf },
    Parse { filename: PathBuf },
}

fn main() -> Result<ExitCode> {
    let args = Args::parse();
    match args.commands {
        Commands::Tokenize { filename } => {
            let input = fs::read_to_string(filename)
                .into_diagnostic()
                .wrap_err_with(|| "reading file".to_string())?;
            Ok(Lexer::new(&input).tokenize_lex())
        }
        Commands::Parse { filename } => {
            let input = fs::read_to_string(filename)
                .into_diagnostic()
                .wrap_err_with(|| "reading file".to_string())?;
            Ok(Parser::new(&input).parse())
        }
    }
}

#[cfg(test)]
mod tests {
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
    fn test_parse_no_file() {
        Command::cargo_bin("loxide")
            .unwrap()
            .arg("parse")
            .arg("text.lox")
            .assert()
            .failure()
            .code(1);
    }
}
