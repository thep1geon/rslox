use crate::interpreter::{self, Interpreter, Env};
use crate::object::Object;
use crate::stmt::Function;
use crate::expr::Lambda;

use std::result::Result;
use ::std::rc::Rc;

pub type NativeFn =
    fn(interpreter: &Interpreter, args: Vec<Object>) -> Result<Object, interpreter::Error>;

#[derive(Debug, Clone)]
pub enum Callable {
    NativeFn(String, u8, NativeFn),
    UserFn(UserFunction),
    LambdaFn(LambdaFunction),
}

impl Callable {
    pub fn name(&self) -> String {
        match self {
            Self::NativeFn(name, _, _) => name.to_string(),
            Self::UserFn(f) => f.decl.name.kind.as_string(),
            Self::LambdaFn(_) => String::from("lambda"),
        }
    }

    pub fn arity(&self) -> u8 {
        match self {
            Self::NativeFn(_, argc, _) => *argc,
            Self::UserFn(f) => f.decl.params.len() as u8,
            Self::LambdaFn(l) => l.lambda.params.len() as u8,
        }
    }

    pub fn call(
        &mut self,
        interpreter: &mut Interpreter,
        args: Vec<Object>,
    ) -> Result<Object, interpreter::Error> {
        match self {
            Self::NativeFn(_, _, func) => func(interpreter, args),
            Self::UserFn(f) => f.call(interpreter, args),
            Self::LambdaFn(l) => l.call(interpreter, args),
        }
    }
}

#[derive(Debug, Clone)]
pub struct UserFunction {
    pub decl: Rc<Function>,
    pub closure: Rc<Env>,
}

impl UserFunction {
    pub fn new(decl: Rc<Function>, closure: Rc<Env>) -> Self {
        Self { decl, closure }
    }

    pub fn call(
        &self,
        interpreter: &mut Interpreter,
        args: Vec<Object>,
    ) -> Result<Object, interpreter::Error> {
        interpreter.push_scope(Rc::clone(&self.closure));
        interpreter.enter_scope();

        for (i, arg) in args.iter().enumerate() {
            interpreter.define(self.decl.params.get(i).unwrap().kind.as_string(), arg.clone());
        }

        let ret = match interpreter.interpret(&self.decl.body) {
            Ok(()) => Ok(Object::Nil),
            Err(e) => match e.kind {
                interpreter::ErrorKind::Return(val) => Ok(val),
                _ => Err(e),
            }
        };

        interpreter.leave_scope();
        interpreter.leave_scope();

        ret
    }
}

#[derive(Debug, Clone)]
pub struct LambdaFunction {
    pub lambda: Rc<Lambda>,
    pub closure: Rc<Env>,
}

impl LambdaFunction {
    pub fn new(lambda: Rc<Lambda>, closure: Rc<Env>) -> Self {
        Self { lambda, closure }
    }

    pub fn call(
        &self,
        interpreter: &mut Interpreter,
        args: Vec<Object>,
    ) -> Result<Object, interpreter::Error> {
        interpreter.push_scope(Rc::clone(&self.closure));
        interpreter.enter_scope();

        for (i, arg) in args.iter().enumerate() {
            interpreter.define(self.lambda.params.get(i).unwrap().kind.as_string(), arg.clone());
        }

        let ret = match interpreter.interpret(&self.lambda.body) {
            Ok(()) => Ok(Object::Nil),
            Err(e) => match e.kind {
                interpreter::ErrorKind::Return(val) => Ok(val),
                _ => Err(e),
            }
        };

        interpreter.leave_scope();
        interpreter.leave_scope();

        ret
    }
}
