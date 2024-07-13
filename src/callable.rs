use crate::interpreter::{self, Interpreter, Env};
use crate::object::Object;
use crate::stmt::Function;
use crate::expr::Lambda;

<<<<<<< HEAD
use std::cell::RefCell;
use std::rc::Rc;
=======
>>>>>>> parent of cfea157 (Finish Ch 12)
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
<<<<<<< HEAD
        args: Vec<Rc<Object>>,
    ) -> Result<Rc<Object>, interpreter::Error> {
        interpreter.enter_scope();
        for closure in self.closures.iter().rev() {
            interpreter.push_scope(Rc::clone(closure));
        }
=======
        args: Vec<Object>,
    ) -> Result<Object, interpreter::Error> {
        interpreter.push_scope(Rc::clone(&self.closure));
        interpreter.enter_scope();
>>>>>>> parent of cfea157 (Finish Ch 12)

        for (i, arg) in args.iter().enumerate() {
            interpreter.define(self.decl.params.get(i).unwrap().kind.as_string(), arg.clone());
        }

        let ret = match interpreter.interpret(&self.decl.body) {
<<<<<<< HEAD
            Ok(()) => {
                if self.is_initializer {
                    return Ok(Rc::clone(
                        self.closures
                            .last()
                            .unwrap()
                            .values
                            .borrow()
                            .get(&"this".to_owned())
                            .unwrap(),
                    ));
                }

                Ok(Rc::new(Object::Nil))
            }
            Err(e) => match e.kind {
                interpreter::ErrorKind::Return(val) => {
                    if self.is_initializer {
                        return Ok(Rc::clone(
                            self.closures
                                .last()
                                .unwrap()
                                .values
                                .borrow()
                                .get(&"this".to_owned())
                                .unwrap(),
                        ));
                    }

                    Ok(val)
                }
=======
            Ok(()) => Ok(Object::Nil),
            Err(e) => match e.kind {
                interpreter::ErrorKind::Return(val) => Ok(val),
>>>>>>> parent of cfea157 (Finish Ch 12)
                _ => Err(e),
            }
        };

        interpreter.leave_scope();
<<<<<<< HEAD
        for _ in self.closures.iter().rev() {
            interpreter.leave_scope();
        }
=======
        interpreter.leave_scope();
>>>>>>> parent of cfea157 (Finish Ch 12)

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
