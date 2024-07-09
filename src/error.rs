use core::fmt;

use crate::{interpreter, lexer, parser};

#[derive(Debug)]
pub enum LoxError {
    Parser(parser::Error),
    Lexer(lexer::Error),
    Runtime(interpreter::Error),
}

pub type LoxResult<T> = std::result::Result<T, LoxError>;

impl fmt::Display for LoxError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Parser(p) => write!(f, "{p:?}"),
            Self::Lexer(l) => write!(f, "{l:?}"),
            Self::Runtime(r) => write!(f, "{r:?}"),
        }
    }
}
