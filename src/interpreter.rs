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

#[derive(Clone, Debug)]
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

pub struct Interpreter {
    envs: Vec<Rc<Env>>,
}

pub enum ErrorKind {
    UndefinedVariable,
    InvalidType,
    IncorrectArgCount,

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
            kind: ErrorKind::Return(obj),
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
            ErrorKind::IncorrectArgCount => write!(f, "IncorrectArgCount"),
            ErrorKind::InvalidType => write!(f, "InvalidType"),
            ErrorKind::UndefinedVariable => write!(f, "UndefinedVariable"),
        }
    }
}

pub type Result<T> = ::std::result::Result<T, Error>;

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

impl Interpreter {
    pub fn new() -> Self {
        let this = Self {
            envs: vec![Rc::new(Env::new())],
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

    pub fn execute(&mut self, stmt: &Stmt) -> Result<()> {
        stmt.accept(self)
    }

    fn eval(&mut self, expr: &Expr) -> Result<Object> {
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

    pub fn define(&self, name: String, obj: Object) {
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
    fn unary(&mut self, expr: &expr::Unary) -> Result<Object> {
        let right = self.eval(&expr.expr)?;

        match expr.op.kind {
            TokenKind::Minus => match self.check_operands(&[&right], &expr.op) {
                Ok(()) => Ok(Object::Number(-right.get_number())),
                Err(e) => Err(e),
            },
            TokenKind::Bang => Ok(Object::Bool(!right.is_truthy())),

            _ => unreachable!(),
        }
    }

    fn binary(&mut self, expr: &expr::Binary) -> Result<Object> {
        let left = self.eval(&expr.left)?;
        let right = self.eval(&expr.right)?;

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

    fn literal(&mut self, expr: &expr::Literal) -> Result<Object> {
        Ok(expr.lit.clone())
    }

    fn grouping(&mut self, expr: &expr::Grouping) -> Result<Object> {
        self.eval(&expr.expr)
    }

    fn var(&mut self, expr: &expr::Var) -> std::result::Result<Object, Error> {
        match self.fetch(&expr.name.kind.as_string()) {
            Some(obj) => Ok(obj.clone()),
            None => Err(Error::undefined_variable(expr.name.line)),
        }
    }

    fn assignment(&mut self, expr: &expr::Assignment) -> std::result::Result<Object, Error> {
        let value = self.eval(&expr.value)?;
        match self.assign(&expr.name.kind.as_string(), value.clone()) {
            Some(()) => Ok(value),
            None => Err(Error::undefined_variable(expr.name.line)),
        }
    }

    fn logical(&mut self, expr: &expr::Logical) -> std::result::Result<Object, Error> {
        let left = self.eval(&expr.left)?;

        if matches!(expr.op.kind, TokenKind::Or) && left.is_truthy() {
            return Ok(left);
        }

        if !left.is_truthy() {
            return Ok(left);
        }

        self.eval(&expr.right)
    }

    fn call(&mut self, expr: &expr::Call) -> std::result::Result<Object, Error> {
        let mut callee = self.eval(&expr.callee)?;

        let mut args = vec![];
        for arg in &expr.args {
            args.push(self.eval(arg)?);
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

    fn lambda(&mut self, expr: &expr::Lambda) -> std::result::Result<Object, Error> {
        let lambda = Callable::LambdaFn(LambdaFunction::new(expr.clone(), self.env()));
        Ok(Object::Func(lambda))
    }
}

impl stmt::Visitor<(), Error> for Interpreter {
    fn print(&mut self, stmt: &stmt::Print) -> std::result::Result<(), Error> {
        let expr = self.eval(&stmt.expr)?;
        println!("{expr}");
        Ok(())
    }

    fn expr(&mut self, stmt: &stmt::Expression) -> std::result::Result<(), Error> {
        self.eval(&stmt.expr)?;
        Ok(())
    }

    fn vardecl(&mut self, stmt: &stmt::VarDecl) -> std::result::Result<(), Error> {
        let value = self.eval(&stmt.initializer)?;
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
        let cond = self.eval(&stmt.condition)?;
        if cond.is_truthy() {
            self.execute(&stmt.then_branch)?;
        } else if let Some(else_b) = &stmt.else_branch {
            self.execute(else_b)?;
        }

        Ok(())
    }

    fn while_stmt(&mut self, stmt: &stmt::While) -> std::result::Result<(), Error> {
        while self.eval(&stmt.condition)?.is_truthy() {
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
        let func = Callable::UserFn(UserFunction::new(stmt.clone(), self.env()));
        self.define(func.name(), Object::Func(func));
        Ok(())
    }

    fn return_stmt(&mut self, stmt: &stmt::Return) -> std::result::Result<(), Error> {
        Err(Error::_return(match &stmt.value {
            Some(val) => self.eval(val)?,
            None => Object::Nil,
        }))
    }

    fn break_stmt(&mut self, _stmt: &stmt::Break) -> std::result::Result<(), Error> {
        Err(Error::_break())
    }
}
