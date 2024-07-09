use core::{fmt, panic};

use crate::callable::Callable;

#[derive(Debug, Clone)]
pub enum Object {
    Nil,
    Func(Callable),
    Bool(bool),
    Number(f64),
    Str(String),
}

impl Object {
    pub fn get_number(&self) -> &f64 {
        match self {
            Self::Number(n) => n,
            _ => panic!("Tried getting a number out of a non number object"),
        }
    }

    pub fn get_func(&mut self) -> &mut Callable {
        match self {
            Self::Func(f) => f,
            _ => panic!("Tried getting a function out of a non function object"),
        }
    }

    pub fn is_truthy(&self) -> bool {
        match *self {
            Object::Nil => false,
            Object::Bool(b) => b,
            _ => true,
        }
    }

    pub fn equals(&self, other: &Object) -> bool {
        match (self, other) {
            (Self::Nil, Self::Nil) => true,
            (Self::Str(s), Self::Str(r)) => s == r,
            (Self::Bool(s), Self::Bool(r)) => s == r,
            (Self::Number(s), Self::Number(r)) => s == r,
            (_, _) => false,
        }
    }
}

impl fmt::Display for Object {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Self::Nil => write!(f, "nil"),
            Self::Func(func) => write!(f, "<fn {}>", func.name()),
            Self::Bool(b) => write!(f, "{b}"),
            Self::Number(n) => write!(f, "{n}"),
            Self::Str(s) => write!(f, "\"{s}\""),
        }
    }
}
