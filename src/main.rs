#![allow(static_mut_refs)]
#![allow(invalid_reference_casting)]

mod backends;
mod codegen;
mod config;
mod error;
mod parser;
mod prelude;
mod reprs;
mod scope;
mod store;
mod tokenizer;
mod type_checker;

use clap::Parser;
use clap_verbosity_flag::InfoLevel;
use dashmap::DashMap;
pub use prelude::*;

use crate::type_checker::type_check;

// a span for denoting a specific portion of code in a specific file
// a span is a range of bytes in a file
// it should also contain the line number and the indent spaces
// it should also contain the column number
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Span {
    /// the start and end character (byte if ASCII) positions in the file
    /// (start..end)
    pub start:  usize,
    pub end:    usize,
    /// the line number and column number of the start position
    pub line:   usize,
    pub col:    usize,
    /// the number of indent spaces at the start position
    pub indent: usize,
    /// the file id in the store
    pub file:   u64,
}

#[derive(Debug, Clone, Copy)]
pub enum Level {
    Error,
    Warning,
    Hint,
}

impl Span {
    pub fn print(&self, msg: impl AsRef<str>, level: Level) {
        let path = store::get_file_path(self.file)
            .strip_prefix(config().root())
            .unwrap_or_else(|_| store::get_file_path(self.file));
        let content = store::get_file_content(self.file);
        let lines: Vec<&str> = content.lines().collect();
        let max_line_num_len = lines.len().to_string().len();
        if self.line == 0 || self.line > lines.len() {
            println!(
                "{}{}{ANSI_END}: {}",
                match level {
                    Level::Error => ANSI_RED,
                    Level::Warning => ANSI_YELLOW,
                    Level::Hint => ANSI_BLUE,
                },
                match level {
                    Level::Error => "error",
                    Level::Warning => "warning",
                    Level::Hint => "hint",
                },
                msg.as_ref(),
            );
            return;
        }
        let line = lines[self.line - 1];
        println!(
            "{}{}{ANSI_END}: {}",
            match level {
                Level::Error => ANSI_RED,
                Level::Warning => ANSI_YELLOW,
                Level::Hint => ANSI_BLUE,
            },
            match level {
                Level::Error => "error",
                Level::Warning => "warning",
                Level::Hint => "hint",
            },
            msg.as_ref(),
        );
        println!(
            "{:>len$}{ANSI_BLUE}-->{ANSI_END} {}:{}:{}",
            "",
            path.display(),
            self.line,
            self.col,
            len = max_line_num_len
        );
        println!(
            "{:>len$} {ANSI_BLUE}|{ANSI_END}",
            "",
            len = max_line_num_len
        );
        let is_single_line = self.end - self.start <= line.len() - (self.col - 1);
        if !is_single_line {
            // find carriage returns in the span
            let span_content = &content[self.start..self.end];
            let carriage_returns = span_content.matches("\r\n").count();
            // multi-line span
            let mut so_far = carriage_returns;
            for (i, line) in lines.iter().enumerate().skip(self.line - 1) {
                println!(
                    "{ANSI_BLUE}{:>len$} |{ANSI_END} {}",
                    i + 1,
                    line,
                    len = max_line_num_len
                );
                if so_far == 0 {
                    so_far += line.len() - (self.col - 1) + 1;
                    // first line
                    println!(
                        "{:>len$} {ANSI_BLUE}|{ANSI_END}{:>width$}{}",
                        "",
                        "",
                        "^".repeat(line.len() - (self.col - 1)),
                        len = max_line_num_len,
                        width = self.col - 2 + max_line_num_len
                    );
                } else if so_far >= self.end - self.start - 1 {
                    // last line
                    let remaining = self.end - self.start - so_far;
                    println!(
                        "{:>len$} {ANSI_BLUE}|{ANSI_END} {}",
                        "",
                        "^".repeat(remaining),
                        len = max_line_num_len
                    );
                    break;
                } else {
                    so_far += line.len() + 1;
                    // middle lines
                    println!(
                        "{:>len$} {ANSI_BLUE}|{ANSI_END} {}",
                        "",
                        "^".repeat(line.len()),
                        len = max_line_num_len
                    );
                }
            }
        } else {
            // single-line span
            println!(
                "{ANSI_BLUE}{:>len$} |{ANSI_END} {}",
                self.line,
                line,
                len = max_line_num_len
            );
            println!(
                "{:>len$} {ANSI_BLUE}|{ANSI_END}{:>width$}{}",
                "",
                "",
                "^".repeat(self.end - self.start),
                len = max_line_num_len,
                width = self.col + max_line_num_len - 2
            );
        }
    }

    pub fn join(&self, other: Span) -> Span {
        if self.file != other.file {
            panic!("cannot join spans from different files");
        }
        Span {
            start:  self.start.min(other.start),
            end:    self.end.max(other.end),
            line:   self.line.min(other.line),
            col:    if self.line < other.line {
                self.col
            } else if self.line > other.line {
                other.col
            } else {
                self.col.min(other.col)
            },
            indent: self.indent.min(other.indent),
            file:   self.file,
        }
    }

    pub fn dummy() -> Span {
        Span {
            start:  0,
            end:    0,
            line:   0,
            col:    0,
            indent: 0,
            file:   0,
        }
    }
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
    /// New project
    New { name: String },
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
    unsafe {
        store::FILE_STORE = Some(store::Store::new());
        scope::TYPE_STORE = Some(store::Store::new());
        scope::VALUE_STORE = Some(store::Store::new());
        scope::SCOPE_STORE = Some(store::Store::new());
        crate::reprs::ir::IR_VALUE_STORE = Some(store::Store::new());
        crate::reprs::ir::IR_VALUE_MAPPING = Some(DashMap::new());
        crate::reprs::ir::IR_TYPE_STORE = Some(store::Store::new());
        crate::reprs::ir::IR_TYPE_MAPPING = Some(DashMap::new());
    }
    let cli = Cli::parse();
    tracing_subscriber::fmt()
        .with_max_level(cli.verbosity)
        .init();

    if let Some(Command::New { name }) = &cli.command {
        if let Err(e) = config::Config::new_project(name) {
            e.log();
            std::process::exit(1);
        }
        println!("Created new project: {}", name);
        return;
    }

    let c = match config::Config::read() {
        Ok(config) => config,
        Err(e) => {
            e.log();
            std::process::exit(1);
        },
    };
    unsafe {
        config::CONFIG = Some(c);
    }

    let src = config().root().join("src");
    if !src.exists() {
        tracing::error!("src directory does not exist");
        std::process::exit(1);
    }
    if !src.is_dir() {
        tracing::error!("src is not a directory");
        std::process::exit(1);
    }
    let main = src.join("main.bay");
    if !main.exists() {
        tracing::error!("src/main.bay does not exist");
        std::process::exit(1);
    }
    if !main.is_file() {
        tracing::error!("src/main.bay is not a file");
        std::process::exit(1);
    }

    fn build(main: &std::path::Path) -> Result<()> {
        let tokens = tokenizer::tokenize(main)?;
        let items = parser::parse(&tokens)?;
        type_check(items)?;
        codegen::codegen()?;
        Ok(())
    }
    match cli.command {
        Some(Command::Build { release: _ }) => {
            if let Err(e) = build(&main) {
                e.log();
                std::process::exit(1);
            }
        },
        Some(Command::Run { release: _ }) => {
            if let Err(e) = build(&main) {
                e.log();
                std::process::exit(1);
            }
            let path = config().root().join("out").join(format!(
                "{}.wasm",
                config().package().name().replace('-', "_")
            ));
            std::process::Command::new("wasmtime")
                .arg(path)
                .status()
                .expect("failed to execute process");
        },
        _ => {
            println!("Not implemented yet");
        },
    }
}
