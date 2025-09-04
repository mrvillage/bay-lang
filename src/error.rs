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
}

impl CompileError {
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
