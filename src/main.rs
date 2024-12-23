use clap::Subcommand;
use codecrafters_interpreter::Lexer;
use miette::{IntoDiagnostic, Result, WrapErr};
use std::fs;
use std::path::PathBuf;
use std::process::ExitCode;

use clap::Parser;

#[derive(Parser, Debug)]
#[command(version, about, long_about = None)]
struct Args {
    #[command(subcommand)]
    commands: Commands,
}
#[derive(Subcommand, Debug)]
enum Commands {
    Tokenize { filename: PathBuf },
}

fn main() -> Result<ExitCode> {
    let args = Args::parse();

    match args.commands {
        Commands::Tokenize { filename } => {
            let input = fs::read_to_string(filename)
                .into_diagnostic()
                .wrap_err_with(|| format!("reading file"))?;
            if !input.is_empty() {
                let lexer = Lexer::new(&input);
                for token in lexer {
                    match token {
                        Ok(t) => println!("{t}"),
                        Err(e) => {
                            println!("{e}");
                            return Ok(ExitCode::from(65));
                        }
                    }
                }
            }
            println!("EOF  null");
            return Ok(ExitCode::from(0));
        }
    }
}
