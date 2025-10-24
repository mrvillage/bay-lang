pub use std::{num::NonZeroU64, str::FromStr};

pub type OptU64 = Option<NonZeroU64>;

pub use crate::{
    config::config,
    error::CompileError,
    reprs::{concrete, hir, token},
    scope::*,
    tokenizer::tokenize,
    Span,
};

pub type Result<T, E = CompileError> = std::result::Result<T, E>;

pub const ANSI_RED: &str = "\x1b[31m";
pub const ANSI_YELLOW: &str = "\x1b[33m";
pub const ANSI_BLUE: &str = "\x1b[34m";
pub const ANSI_END: &str = "\x1b[0m";
