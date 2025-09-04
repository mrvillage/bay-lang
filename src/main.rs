mod config;
mod error;
mod parser;
mod prelude;
mod reprs;
mod store;
mod tokenize;

use clap::Parser;
use clap_verbosity_flag::InfoLevel;
use prelude::*;

// a span for denoting a specific portion of code in a specific file
// a span is a range of bytes in a file
// it should also contain the line number and the indent spaces
// it should also contain the column number
#[derive(Debug, Clone)]
pub struct Span {
    pub start:  usize,
    pub end:    usize,
    pub line:   usize,
    pub col:    usize,
    pub indent: usize,
    pub file:   u64,
}

#[derive(Debug, Parser)]
struct Cli {
    #[command(flatten)]
    verbosity: clap_verbosity_flag::Verbosity<InfoLevel>,
    #[command(subcommand)]
    command:   Option<Command>,
}

#[derive(Debug, Parser)]
enum Command {
    /// Compile the project
    Build {
        #[arg(short, long)]
        release: bool,
    },
    /// Run the project
    Run {
        #[arg(short, long)]
        release: bool,
    },
    /// Test the project
    Test {
        #[arg(short, long)]
        release: bool,
    },
    /// Check the project
    Check {
        #[arg(short, long)]
        release: bool,
    },
    /// Format the project
    Format {
        #[arg(short, long)]
        release: bool,
    },
}

fn main() {
    let cli = Cli::parse();

    tracing_subscriber::fmt()
        .with_max_level(cli.verbosity)
        .init();

    let config = match config::Config::read() {
        Ok(config) => config,
        Err(e) => {
            e.log();
            std::process::exit(1);
        },
    };

    tracing::trace!("{:#?}", config);

    let src = config.root().join("src");
    if !src.exists() {
        tracing::error!("src directory does not exist");
        std::process::exit(1);
    }
    if !src.is_dir() {
        tracing::error!("src is not a directory");
        std::process::exit(1);
    }
    let src = src.join("main.bay");
    if !src.exists() {
        tracing::error!("src/main.bay does not exist");
        std::process::exit(1);
    }
    if !src.is_file() {
        tracing::error!("src/main.bay is not a file");
        std::process::exit(1);
    }
}
