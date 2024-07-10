use core::fmt;

use crate::{interpreter, lexer, parser};

pub enum LoxError {
    Lexer(lexer::Error),
    Parser(parser::Error),
    Runtime(interpreter::Error),
}

impl LoxError {
    fn line(&self) -> usize {
        match self {
            Self::Lexer(l) => l.line,
            Self::Parser(p) => p.tok.line,
            Self::Runtime(r) => r.line,
        }
    }
}

pub type LoxResult<T> = std::result::Result<T, LoxError>;

impl fmt::Display for LoxError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "[Line:{}] Error: ", self.line())?;

        match self {
            Self::Parser(p) => write!(f, "{p}"),
            Self::Lexer(l) => write!(f, "{l}"),
            Self::Runtime(r) => write!(f, "{r}"),
        }
    }
}
