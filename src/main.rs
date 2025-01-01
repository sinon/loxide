use clap::Subcommand;
use loxide::lexer::{Lexer, Token};
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
    let mut exit_code = 0;
    match args.commands {
        Commands::Tokenize { filename } => {
            let input = fs::read_to_string(filename)
                .into_diagnostic()
                .wrap_err_with(|| "reading file".to_string())?;
            if !input.is_empty() {
                let lexer = Lexer::new(&input);
                for token in lexer {
                    match token {
                        Ok(t) => println!("{t}"),
                        Err(e) => {
                            eprintln!("{e}");
                            exit_code = 65;
                            continue;
                        }
                    }
                }
            }
            println!("EOF  null");
            Ok(ExitCode::from(exit_code))
        }
        Commands::Parse { filename } => {
            let _input = fs::read_to_string(filename)
                .into_diagnostic()
                .wrap_err_with(|| "reading file".to_string())?;
            // let tokens: Vec<Result<Token, MError>> = Lexer::new(&_input).into_iter().collect();
            // for token in tokens {
            //     match token {
            //         Ok(_) => {}
            //         Err(e) => {
            //             // eprintln!("{e}");
            //             // exit_code = 65;
            //             continue;
            //         }
            //     }
            // }
            let tokens: Vec<Token> = Lexer::new(&_input)
                .into_iter()
                .filter_map(Result::ok)
                .collect();
            let parser = Parser::new(tokens);
            for exp in parser {
                match exp {
                    Ok(e) => {
                        println!("{e}");
                    }
                    Err(_) => {
                        exit_code = 65;
                        continue;
                    }
                }
            }
            Ok(ExitCode::from(exit_code))
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
            .stdout(format!("loxide 0.1.0\n"))
            .stderr("");

        Command::cargo_bin("loxide")
            .unwrap()
            .arg("-V")
            .assert()
            .success()
            .code(0)
            .stdout(format!("loxide 0.1.0\n"))
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
