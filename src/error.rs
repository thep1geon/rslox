use core::fmt;

use crate::{interpreter, lexer, parser, resolver};

pub enum LoxError {
    Lexer(lexer::Error),
    Parser(parser::Error),
    Resolver(resolver::Error),
    Runtime(interpreter::Error),
}

impl LoxError {
    fn line(&self) -> usize {
        match self {
            Self::Lexer(l) => l.line,
            Self::Parser(p) => p.tok.line,
            Self::Resolver(r) => r.tok.line,
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
            Self::Resolver(r) => write!(f, "{r}"),
            Self::Runtime(r) => write!(f, "{r}"),
        }
    }
}
