use clap::Subcommand;
use loxide::interpreter::Interpreter;
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
    Run { filename: PathBuf },
}

fn main() -> Result<ExitCode> {
    let args = Args::parse();
    match args.commands {
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
