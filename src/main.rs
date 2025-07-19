use clap::Subcommand;
use loxide::eval::Eval;
use loxide::eval_parser::Parser;
use loxide::interpreter::Interpreter;
use loxide::lexer::Lexer;
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
    Evaluate { filename: PathBuf },
    Run { filename: PathBuf },
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
        Commands::Evaluate { filename } => {
            let input = fs::read_to_string(filename)
                .into_diagnostic()
                .wrap_err_with(|| "reading file".to_string())?;
            let mut exit_code = 0;
            for res in Eval::new(&input) {
                match res {
                    Ok(r) => println!("{r}"),
                    Err(_) => exit_code = 70,
                }
            }
            Ok(ExitCode::from(exit_code))
        }
        Commands::Run { filename } => {
            let input = fs::read_to_string(filename)
                .into_diagnostic()
                .wrap_err_with(|| "reading file".to_string())?;
            let mut exit_code = 0;
            for res in Interpreter::new(&input) {
                match res {
                    Ok(()) => {}
                    Err(err_code) => {
                        exit_code = err_code;
                        break;
                    }
                }
            }
            Ok(ExitCode::from(exit_code))
        }
    }
}
