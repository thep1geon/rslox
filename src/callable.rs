use crate::class::{Class, Instance};
use crate::expr::Lambda;
use crate::interpreter::{self, Env, Interpreter};
use crate::object::Object;
use crate::stmt::Function;

use std::rc::Rc;
use std::cell::RefCell;
use std::result::Result;

pub type NativeFn =
    fn(interpreter: &Interpreter, args: Vec<Rc<Object>>) -> Result<Rc<Object>, interpreter::Error>;

#[derive(Debug, Clone)]
pub enum Callable {
    Native(String, u8, NativeFn),
    User(UserFunction),
    Lambda(LambdaFunction),
    Class(Class),
}

impl Callable {
    pub fn name(&self) -> String {
        match self {
            Self::Native(name, _, _) => name.to_string(),
            Self::User(f) => f.decl.name.kind.as_string(),
            Self::Lambda(_) => String::from("lambda"),
            Self::Class(c) => c.name.kind.as_string(),
        }
    }

    pub fn arity(&self) -> u8 {
        match self {
            Self::Native(_, argc, _) => *argc,
            Self::User(f) => f.decl.params.len() as u8,
            Self::Lambda(l) => l.lambda.params.len() as u8,
            Self::Class(c) => c.arity(),
        }
    }

    pub fn call(
        &self,
        interpreter: &mut Interpreter,
        args: Vec<Rc<Object>>,
    ) -> Result<Rc<Object>, interpreter::Error> {
        match self {
            Self::Native(_, _, func) => func(interpreter, args),
            Self::User(f) => f.call(interpreter, args),
            Self::Lambda(l) => l.call(interpreter, args),
            Self::Class(c) => c.call(interpreter, args),
        }
    }
}

#[derive(Debug, Clone)]
pub struct UserFunction {
    pub decl: Rc<Function>,
    pub closures: Vec<Rc<Env>>,
    is_initializer: bool,
}

impl UserFunction {
    pub fn new(decl: Rc<Function>, closures: Vec<Rc<Env>>, is_initializer: bool) -> Self {
        Self {
            decl,
            closures,
            is_initializer,
        }
    }

    pub fn call(
        &self,
        interpreter: &mut Interpreter,
        args: Vec<Rc<Object>>,
    ) -> Result<Rc<Object>, interpreter::Error> {
        for closure in &self.closures {
            interpreter.push_scope(Rc::clone(closure));
        }
        interpreter.enter_scope();

        for (i, arg) in args.iter().enumerate() {
            interpreter.define(
                self.decl.params.get(i).unwrap().kind.as_string(),
                arg.clone(),
            );
        }

        let ret = match interpreter.interpret(&self.decl.body) {
            Ok(()) => {
                if self.is_initializer {
                    return Ok(Rc::clone(self
                        .closures
                        .last()
                        .unwrap()
                        .values
                        .borrow()
                        .get(&"this".to_owned())
                        .unwrap()
                    ));
                }

                Ok(Rc::new(Object::Nil))
            }
            Err(e) => match e.kind {
                interpreter::ErrorKind::Return(val) => {
                    if self.is_initializer {
                        return Ok(Rc::clone(self
                            .closures
                            .last()
                            .unwrap()
                            .values
                            .borrow()
                            .get(&"this".to_owned())
                            .unwrap()
                        ));
                    }

                    Ok(val)
                },
                _ => Err(e),
            },
        };

        for _ in self.closures.iter().rev() {
            interpreter.leave_scope();
        }
        interpreter.leave_scope();

        ret
    }

    pub fn bind(&self, instance: Rc<RefCell<Instance>>) -> Self {
        let mut closures = self.closures.clone();
        closures.push(Rc::new(Env::new()));
        closures.last_mut().unwrap().values.borrow_mut().insert(
            "this".to_owned(),
            Rc::new(Object::Instance(Rc::clone(&instance))),
        );
        Self::new(Rc::clone(&self.decl), closures, self.is_initializer)
    }
}

#[derive(Debug, Clone)]
pub struct LambdaFunction {
    pub lambda: Rc<Lambda>,
    pub closures: Vec<Rc<Env>>,
}

impl LambdaFunction {
    pub fn new(lambda: Rc<Lambda>, closures: Vec<Rc<Env>>) -> Self {
        Self { lambda, closures }
    }

    pub fn call(
        &self,
        interpreter: &mut Interpreter,
        args: Vec<Rc<Object>>,
    ) -> Result<Rc<Object>, interpreter::Error> {
        for closure in &self.closures {
            interpreter.push_scope(Rc::clone(closure));
        }
        interpreter.enter_scope();

        for (i, arg) in args.iter().enumerate() {
            interpreter.define(
                self.lambda.params.get(i).unwrap().kind.as_string(),
                arg.clone(),
            );
        }

        let ret = match interpreter.interpret(&self.lambda.body) {
            Ok(()) => Ok(Rc::new(Object::Nil)),
            Err(e) => match e.kind {
                interpreter::ErrorKind::Return(val) => Ok(val),
                _ => Err(e),
            },
        };

        for _ in self.closures.iter().rev() {
            interpreter.leave_scope();
        }
        interpreter.leave_scope();

        ret
    }
}
