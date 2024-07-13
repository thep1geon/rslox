use std::cell::RefCell;
use std::collections::HashMap;
use std::fmt;
use std::rc::Rc;
use std::time::{SystemTime, UNIX_EPOCH};

use crate::callable::{Callable, LambdaFunction, UserFunction};
use crate::expr::{self, Expr};
use crate::object::Object;
use crate::stmt::{self, Stmt};
use crate::token::Token;
use crate::token_kind::TokenKind;

pub enum ErrorKind {
    InvalidType,
    UndefinedVariable,
<<<<<<< HEAD
    UndefinedProperty,
    IncorrectArgCount,
    SuperClassNotClass,
    TypeHasNoProperties,
=======
    InvalidType,
    IncorrectArgCount,
>>>>>>> parent of cfea157 (Finish Ch 12)

    Return(Object),
    Break,
}

pub struct Error {
    pub kind: ErrorKind,
    pub line: usize,
}

impl Error {
    pub fn undefined_variable(line: usize) -> Self {
        Self {
            kind: ErrorKind::UndefinedVariable,
            line,
        }
    }

    pub fn invalid_type(line: usize) -> Self {
        Self {
            kind: ErrorKind::InvalidType,
            line,
        }
    }

    pub fn incorrect_arg_count(line: usize) -> Self {
        Self {
            kind: ErrorKind::IncorrectArgCount,
            line,
        }
    }
    pub fn _return(obj: Object) -> Self {
        Self {
<<<<<<< HEAD
            kind: ErrorKind::TypeHasNoProperties,
            line,
        }
    }

    pub fn super_class_not_class(line: usize) -> Self {
        Self {
            kind: ErrorKind::SuperClassNotClass,
            line,
        }
    }

    pub fn _return(obj: Rc<Object>) -> Self {
        Self {
            kind: ErrorKind::Return(Rc::clone(&obj)),
=======
            kind: ErrorKind::Return(obj),
>>>>>>> parent of cfea157 (Finish Ch 12)
            line: 0,
        }
    }
    pub fn _break() -> Self {
        Self {
            kind: ErrorKind::Break,
            line: 0,
        }
    }
}

impl fmt::Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self.kind {
            ErrorKind::Return(_) => write!(f, "Return"),
            ErrorKind::Break => write!(f, "Break"),
            ErrorKind::InvalidType => write!(f, "InvalidType"),
            ErrorKind::UndefinedVariable => write!(f, "UndefinedVariable"),
<<<<<<< HEAD
            ErrorKind::IncorrectArgCount => write!(f, "IncorrectArgCount"),
            ErrorKind::UndefinedProperty => write!(f, "UndefinedProperty"),
            ErrorKind::SuperClassNotClass => write!(f, "SuperClassNotClass"),
            ErrorKind::TypeHasNoProperties => write!(f, "TypeHasNoProperties"),
=======
>>>>>>> parent of cfea157 (Finish Ch 12)
        }
    }
}

pub type Result<T> = ::std::result::Result<T, Error>;

#[derive(Debug, Clone)]
pub struct Env {
    pub values: RefCell<HashMap<String, Object>>,
}

impl Env {
    pub fn new() -> Self {
        Self {
            values: RefCell::new(HashMap::new()),
        }
    }
}

fn native_clock_fn(_interpreter: &Interpreter, _args: Vec<Object>) -> Result<Object> {
    let time = SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .expect("Did we travel back in time?")
        .as_secs() as f64;
    Ok(Object::Number(time))
}

fn native_println_fn(_interpreter: &Interpreter, args: Vec<Object>) -> Result<Object> {
    for arg in args {
        match arg {
            Object::Str(s) => print!("{s}"),
            _ => print!("{arg}"),
        }
    }

    println!();

    Ok(Object::Nil)
}

pub struct Interpreter {
    envs: Vec<Rc<Env>>,
    locals: HashMap<Rc<Expr>, usize>,
}


impl Interpreter {
    pub fn new() -> Self {
        let mut this = Self {
            envs: vec![Rc::new(Env::new())],
            locals: HashMap::new(),
        };

        this.define(
            String::from("clock"),
            Object::Func(Callable::NativeFn(
                String::from("clock"),
                0,
                native_clock_fn,
            )),
        );

        this.define(
            String::from("println"),
            Object::Func(Callable::NativeFn(
                String::from("println"),
                1,
                native_println_fn,
            )),
        );

        this
    }

    pub fn interpret(&mut self, stmts: &Vec<Stmt>) -> Result<()> {
        for stmt in stmts {
            self.execute(stmt)?;
        }

        Ok(())
    }

    fn execute(&mut self, stmt: &Stmt) -> Result<()> {
        stmt.accept(self)
    }

    pub fn resolve(&mut self, expr: Rc<Expr>, depth: usize) {
        self.locals.insert(expr, depth);
    } 

    fn lookup_variable(&self, expr: Rc<Expr>, name: &Token) -> Result<Object> {
        let var_name = &name.kind.as_string();
        match self.locals.get(&expr) {
            Some(dist) => self.env_at_distance(*dist).values.borrow().get(var_name).ok_or(Error::undefined_variable(name.line)).cloned(),
            None => self.global_env().values.borrow().get(var_name).ok_or(Error::undefined_variable(name.line)).cloned(),
        }
        
    }

    fn eval(&mut self, expr: Rc<Expr>) -> Result<Object> {
        expr.accept(self)
    }

    fn check_operands(&self, objs: &[&Object], op: &Token) -> Result<()> {
        for obj in objs {
            if !matches!(obj, Object::Number(_)) {
                return Err(Error::invalid_type(op.line));
            }
        }

        Ok(())
    }

    pub fn define(&mut self, name: String, obj: Object) {
        _ = self.env().values.borrow_mut().insert(name.clone(), obj);
    }

    pub fn assign(&self, name: &String, obj: Object) -> Option<()> {
        for env in self.envs.iter().rev() {
            if !env.values.borrow().contains_key(name) {
                continue;
            }

            env.values.borrow_mut().insert(name.clone(), obj);
            return Some(());
        }

        None
    }

    pub fn fetch(&self, name: &String) -> Option<Object> {
        for env in self.envs.iter().rev() {
            if !env.values.borrow().contains_key(name) {
                continue;
            }

            return env.values.borrow().get(name).cloned();
        }

        None
    }

    fn env(&self) -> Rc<Env> {
        Rc::clone(
            self.envs
                .last()
                .expect("Where the heck did the global env go?!?!?"),
        )
    }

    fn env_at_distance(&self, dist: usize) -> Rc<Env> {
        Rc::clone(
            self.envs
                .get(self.envs.len() - dist)
                .expect("I don't even know how this went wrong"),
        )
    }

    fn global_env(&self) -> Rc<Env> {
        Rc::clone(
            self.envs
                .first()
                .expect("Where the heck did the global env go?!?!?"),
        )
    }

    pub fn push_scope(&mut self, env: Rc<Env>) {
        self.envs.push(env);
    }

    pub fn enter_scope(&mut self) {
        self.envs.push(Rc::new(Env::new()));
    }

    pub fn leave_scope(&mut self) -> Rc<Env> {
        self.envs
            .pop()
            .expect("The global scope dissapeared again :|")
    }
}

impl expr::Visitor<Object, Error> for Interpreter {
    fn unary(&mut self, expr: Rc<expr::Unary>) -> Result<Object> {
        let right = self.eval(Rc::clone(&expr.expr))?;

        match expr.op.kind {
            TokenKind::Minus => match self.check_operands(&[&right], &expr.op) {
                Ok(()) => Ok(Object::Number(-right.get_number())),
                Err(e) => Err(e),
            },
            TokenKind::Bang => Ok(Object::Bool(!right.is_truthy())),

            _ => unreachable!(),
        }
    }

    fn binary(&mut self, expr: Rc<expr::Binary>) -> Result<Object> {
        let left = self.eval(Rc::clone(&expr.left))?;
        let right = self.eval(Rc::clone(&expr.right))?;

        match expr.op.kind {
            TokenKind::Minus => match self.check_operands(&[&left, &right], &expr.op) {
                Ok(()) => Ok(Object::Number(left.get_number() - right.get_number())),
                Err(e) => Err(e),
            },
            TokenKind::Plus => {
                use Object::{Number, Str};
                match (left, right) {
                    (Number(l), Number(r)) => Ok(Object::Number(l + r)),
                    (Str(l), Str(r)) => Ok(Object::Str(l.to_owned() + &r)),
                    (Number(l), Str(r)) => Ok(Object::Str(format!("{l}{r}").to_string())),
                    (Str(l), Number(r)) => Ok(Object::Str(format!("{l}{r}").to_string())),
                    (_, _) => Err(Error::invalid_type(expr.op.line)),
                }
            }
            TokenKind::Slash => match self.check_operands(&[&left, &right], &expr.op) {
                Ok(()) => Ok(Object::Number(left.get_number() / right.get_number())),
                Err(e) => Err(e),
            },
            TokenKind::Star => match self.check_operands(&[&left, &right], &expr.op) {
                Ok(()) => Ok(Object::Number(left.get_number() * right.get_number())),
                Err(e) => Err(e),
            },

            TokenKind::Gt => match self.check_operands(&[&left, &right], &expr.op) {
                Ok(()) => Ok(Object::Bool(left.get_number() > right.get_number())),
                Err(e) => Err(e),
            },
            TokenKind::GtEq => match self.check_operands(&[&left, &right], &expr.op) {
                Ok(()) => Ok(Object::Bool(left.get_number() >= right.get_number())),
                Err(e) => Err(e),
            },
            TokenKind::Lt => match self.check_operands(&[&left, &right], &expr.op) {
                Ok(()) => Ok(Object::Bool(left.get_number() < right.get_number())),
                Err(e) => Err(e),
            },
            TokenKind::LtEq => match self.check_operands(&[&left, &right], &expr.op) {
                Ok(()) => Ok(Object::Bool(left.get_number() <= right.get_number())),
                Err(e) => Err(e),
            },

            TokenKind::BangEq => Ok(Object::Bool(left.equals(&right))),
            TokenKind::EqEq => Ok(Object::Bool(left.equals(&right))),

            _ => unreachable!(),
        }
    }

    fn literal(&mut self, expr: Rc<expr::Literal>) -> Result<Object> {
        Ok(expr.lit.clone())
    }

    fn grouping(&mut self, expr: Rc<expr::Grouping>) -> Result<Object> {
        self.eval(Rc::clone(&expr.expr))
    }

    fn var(&mut self, expr: Rc<expr::Var>) -> std::result::Result<Object, Error> {
        self.lookup_variable(Rc::new(Expr::Var(Rc::clone(&expr))), &expr.name)
    }

    fn assignment(&mut self, expr: Rc<expr::Assignment>) -> std::result::Result<Object, Error> {
        let value = self.eval(Rc::clone(&expr.value))?;
        let var_name = expr.name.kind.as_string();
        let expr = Rc::new(Expr::Assignment(Rc::clone(&expr)));

        _ = match self.locals.get(&expr) {
            Some(dist) => self.env_at_distance(*dist).values.borrow_mut().insert(var_name, value.clone()),
            None => self.global_env().values.borrow_mut().insert(var_name, value.clone()),
        };

        Ok(value)
    }

    fn logical(&mut self, expr: Rc<expr::Logical>) -> std::result::Result<Object, Error> {
        let left = self.eval(Rc::clone(&expr.left))?;

        if matches!(expr.op.kind, TokenKind::Or) && left.is_truthy() {
            return Ok(left);
        }

        if !left.is_truthy() {
            return Ok(left);
        }

        self.eval(Rc::clone(&expr.right))
    }

    fn call(&mut self, expr: Rc<expr::Call>) -> std::result::Result<Object, Error> {
        let mut callee = self.eval(Rc::clone(&expr.callee))?;

        let mut args = vec![];
        for arg in &expr.args {
            args.push(self.eval(Rc::clone(arg))?);
        }

        if !matches!(callee, Object::Func(_)) {
            return Err(Error::invalid_type(expr.paren.line));
        }

        let func: &mut Callable = callee.get_func();

        if func.arity() as usize != args.len() {
            return Err(Error::incorrect_arg_count(expr.paren.line));
        }

        func.call(self, args)
    }

    fn lambda(&mut self, expr: Rc<expr::Lambda>) -> std::result::Result<Object, Error> {
        let lambda = Callable::LambdaFn(LambdaFunction::new(Rc::clone(&expr), self.env()));
        Ok(Object::Func(lambda))
    }

    fn superclass(&mut self, expr: Rc<expr::Super>) -> std::result::Result<Rc<Object>, Error> {
        let dist = *self
            .locals
            .get(&Rc::new(Expr::Super(Rc::clone(&expr))))
            .unwrap();

        let superclass = match self
            .env_at_distance(dist)
            .values
            .borrow()
            .get(&"super".to_owned())
            .unwrap()
            .as_ref()
        {
            Object::Func(Callable::Class(c)) => c.clone(),
            _ => unreachable!(),
        };

        let instance = match self
            .env_at_distance(dist - 1)
            .values
            .borrow()
            .get(&"this".to_owned())
            .unwrap()
            .as_ref()
        {
            Object::Instance(i) => i.clone(),
            _ => unreachable!(),
        };

        let method = match superclass.find_method(&expr.method.kind.as_string()) {
            Some(m) => m,
            None => return Err(Error::undefined_property(expr.method.line)),
        };

        Ok(Rc::new(Object::Func(Callable::User(method.bind(instance)))))
    }
}

impl stmt::Visitor<(), Error> for Interpreter {
    fn print(&mut self, stmt: &stmt::Print) -> std::result::Result<(), Error> {
        let expr = self.eval(Rc::clone(&stmt.expr))?;
        println!("{expr}");
        Ok(())
    }

    fn expr(&mut self, stmt: &stmt::Expression) -> std::result::Result<(), Error> {
        self.eval(Rc::clone(&stmt.expr))?;
        Ok(())
    }

    fn vardecl(&mut self, stmt: &stmt::VarDecl) -> std::result::Result<(), Error> {
        let value = self.eval(Rc::clone(&stmt.initializer))?;
        self.define(stmt.name.kind.as_string(), value);
        Ok(())
    }

    fn block(&mut self, stmt: &stmt::Block) -> std::result::Result<(), Error> {
        self.enter_scope();

        self.interpret(&stmt.statements)?;

        self.leave_scope();

        Ok(())
    }

    fn if_stmt(&mut self, stmt: &stmt::If) -> std::result::Result<(), Error> {
        let cond = self.eval(Rc::clone(&stmt.condition))?;
        if cond.is_truthy() {
            self.execute(&stmt.then)?;
        } else if let Some(else_b) = &stmt.else_ {
            self.execute(else_b)?;
        }

        Ok(())
    }

    fn while_stmt(&mut self, stmt: &stmt::While) -> std::result::Result<(), Error> {
        while self.eval(Rc::clone(&stmt.condition))?.is_truthy() {
            match self.execute(&stmt.statement) {
                Ok(()) => {},
                Err(e) => match &e.kind {
                    ErrorKind::Break => break,
                    _ => return Err(e),
                }
            }
        }

        Ok(())
    }

    fn function(&mut self, stmt: &stmt::Function) -> std::result::Result<(), Error> {
<<<<<<< HEAD
        let func = Callable::User(UserFunction::new(
            Rc::new(stmt.clone()),
            vec![self.env()],
            false,
        ));
        self.define(func.name(), Rc::new(Object::Func(func)));
=======
        let func = Callable::UserFn(UserFunction::new(Rc::new(stmt.clone()), self.env()));
        self.define(func.name(), Object::Func(func));
>>>>>>> parent of cfea157 (Finish Ch 12)
        Ok(())
    }

    fn return_stmt(&mut self, stmt: &stmt::Return) -> std::result::Result<(), Error> {
        Err(Error::_return(match &stmt.value {
            Some(val) => self.eval(Rc::clone(val))?,
            None => Object::Nil,
        }))
    }

    fn break_stmt(&mut self, _stmt: &stmt::Break) -> std::result::Result<(), Error> {
        Err(Error::_break())
    }
<<<<<<< HEAD

    fn class_decl(&mut self, stmt: &stmt::ClassDecl) -> std::result::Result<(), Error> {
        let superclass_obj = match &stmt.superclass {
            Some(s) => self.eval(Rc::new(Expr::Var(Rc::clone(s))))?,
            None => Rc::new(Object::Nil),
        };

        let superclass = match superclass_obj.as_ref() {
            Object::Nil => None,
            Object::Func(Callable::Class(c)) => Some(Rc::new(c.clone())),
            _ => return Err(Error::super_class_not_class(stmt.name.line)),
        };

        self.define(stmt.name.kind.as_string(), Rc::new(Object::Nil));

        if stmt.superclass.is_some() {
            self.enter_scope();
            self.define("super".to_owned(), superclass_obj);
        }

        let mut methods: HashMap<String, UserFunction> = HashMap::new();
        for method in &stmt.methods {
            let function = UserFunction::new(
                Rc::new(method.clone()),
                vec![self.env().clone()],
                method.name.kind.as_string() == "init",
            );
            methods.insert(method.name.kind.as_string(), function);
        }

        let class = Class::new(stmt.name.clone(), superclass, methods);

        if stmt.superclass.is_some() {
            self.leave_scope();
        }

        self.assign(
            &stmt.name.kind.as_string(),
            Rc::new(Object::Func(Callable::Class(class))),
        );
        Ok(())
    }
=======
>>>>>>> parent of cfea157 (Finish Ch 12)
}
