pub use std::{str::FromStr, sync::Arc};

pub use crate::{error::CompileError, reprs::token::*, tokenize::tokenize, Span};

pub type Result<T, E = CompileError> = std::result::Result<T, E>;
