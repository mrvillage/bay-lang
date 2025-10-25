use crate::Span;

#[derive(Debug)]
#[non_exhaustive]
pub enum CompileError {
    /// Could not find the config file in any parent directory
    ConfigNotFound,
    /// Could not read the config file
    ConfigReadError(toml::de::Error),
    /// Invalid configuration
    InvalidConfig(&'static str),
    /// IO error
    IoError(std::io::Error),
    /// Unexpected character
    UnexpectedCharacter(Span, char),
    /// Unexpected end of file
    UnexpectedEndOfFile(Span, char),
    /// Unexpected token
    UnexpectedToken(Span, Option<String>),
    /// Unexpected end of input
    UnexpectedEndOfInput(Span),
    /// Unterminated comment
    UnterminatedComment(Span),
    /// Arbitrary error with a message
    Error(Span, String),
    /// Unimplemented feature
    Unimplemented(Span, &'static str),
    /// No main function found
    NoMainFunction,
}

impl CompileError {
    pub fn error(span: Span, msg: impl Into<String>) -> Self {
        CompileError::Error(span, msg.into())
    }

    pub fn log(&self) {
        match self {
            CompileError::ConfigNotFound => {
                match std::env::current_dir() {
                    Ok(path) => {
                        tracing::error!(
                            "could not find `bay.toml` in {} or any parent directory",
                            path.display()
                        )
                    },
                    Err(_) => {
                        tracing::error!("could not find `bay.toml` in any parent directory");
                    },
                }
            },
            CompileError::ConfigReadError(err) => {
                tracing::error!("could not read `bay.toml`: {}", err);
            },
            CompileError::InvalidConfig(err) => {
                tracing::error!("invalid configuration: {}", err);
            },
            CompileError::IoError(err) => {
                tracing::error!("io error: {}", err);
            },
            CompileError::UnexpectedCharacter(span, c) => {
                span.print(format!("unexpected character '{}'", c), crate::Level::Error);
            },
            CompileError::UnexpectedEndOfFile(span, c) => {
                span.print(
                    if *c == '\0' {
                        "unexpected end of file".to_string()
                    } else {
                        format!("unexpected end of file, expected '{}'", c)
                    },
                    crate::Level::Error,
                );
            },
            CompileError::UnexpectedToken(span, expected) => {
                span.print(
                    if let Some(expected) = expected {
                        format!("unexpected token, expected {}", expected)
                    } else {
                        "unexpected token".to_string()
                    },
                    crate::Level::Error,
                );
            },
            CompileError::UnexpectedEndOfInput(span) => {
                span.print("unexpected end of input", crate::Level::Error);
            },
            CompileError::UnterminatedComment(span) => {
                span.print("unterminated comment", crate::Level::Error);
            },
            CompileError::Error(span, msg) => {
                span.print(msg, crate::Level::Error);
            },
            CompileError::Unimplemented(span, msg) => {
                span.print(format!("unimplemented: {}", msg), crate::Level::Error);
            },
            CompileError::NoMainFunction => {
                tracing::error!("no main function found");
            },
        }
    }
}

impl From<toml::de::Error> for CompileError {
    fn from(err: toml::de::Error) -> Self {
        CompileError::ConfigReadError(err)
    }
}

impl From<std::io::Error> for CompileError {
    fn from(err: std::io::Error) -> Self {
        CompileError::IoError(err)
    }
}
