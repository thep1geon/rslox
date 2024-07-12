use crate::object::Object;
use crate::token::Token;
use crate::stmt::Stmt;

use std::ptr;
use std::result::Result;
use std::rc::Rc;
use std::hash::{Hash, Hasher};

pub trait Visitor<T, E> {
    fn assignment(&mut self, expr: Rc<Assignment>) -> Result<T, E>;
    fn binary(&mut self, expr: Rc<Binary>) -> Result<T, E>;
    fn call(&mut self, expr: Rc<Call>) -> Result<T, E>;
    fn grouping(&mut self, expr: Rc<Grouping>) -> Result<T, E>;
    fn literal(&mut self, expr: Rc<Literal>) -> Result<T, E>;
    fn unary(&mut self, expr: Rc<Unary>) -> Result<T, E>;
    fn var(&mut self, expr: Rc<Var>) -> Result<T, E>;
    fn logical(&mut self, expr: Rc<Logical>) -> Result<T, E>;
    fn lambda(&mut self, expr: Rc<Lambda>) -> Result<T, E>;
}

#[derive(Debug, Clone)]
pub enum Expr {
    Assignment(Rc<Assignment>),
    Binary(Rc<Binary>),
    Call(Rc<Call>),
    Unary(Rc<Unary>),
    Literal(Rc<Literal>),
    Var(Rc<Var>),
    Grouping(Rc<Grouping>),
    Logical(Rc<Logical>),
    Lambda(Rc<Lambda>),
}

impl Expr {
    pub fn accept<T, E>(&self, visitor: &mut dyn Visitor<T, E>) -> Result<T, E> {
        match self {
            Expr::Assignment(a) => visitor.assignment(Rc::clone(a)),
            Expr::Binary(b)     => visitor.binary(Rc::clone(b)),
            Expr::Call(c)       => visitor.call(Rc::clone(c)),
            Expr::Grouping(g)   => visitor.grouping(Rc::clone(g)),
            Expr::Literal(l)    => visitor.literal(Rc::clone(l)),
            Expr::Unary(u)      => visitor.unary(Rc::clone(u)),
            Expr::Var(v)        => visitor.var(Rc::clone(v)),
            Expr::Logical(l)    => visitor.logical(Rc::clone(l)),
            Expr::Lambda(l)     => visitor.lambda(Rc::clone(l)),
        }
    }
}

impl PartialEq for Expr {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Expr::Assignment(a), Expr::Assignment(b)) => Rc::ptr_eq(a, b), 
            (Expr::Binary(a), Expr::Binary(b)) => Rc::ptr_eq(a, b), 
            (Expr::Call(a), Expr::Call(b)) => Rc::ptr_eq(a, b), 
            (Expr::Grouping(a), Expr::Grouping(b)) => Rc::ptr_eq(a, b), 
            (Expr::Literal(a), Expr::Literal(b)) => Rc::ptr_eq(a, b), 
            (Expr::Unary(a), Expr::Unary(b)) => Rc::ptr_eq(a, b), 
            (Expr::Var(a), Expr::Var(b)) => Rc::ptr_eq(a, b), 
            (Expr::Logical(a), Expr::Logical(b)) => Rc::ptr_eq(a, b), 
            (Expr::Lambda(a), Expr::Lambda(b)) => Rc::ptr_eq(a, b), 
            (_, _) => false,
        }
    }
}

impl Eq for Expr {}

impl Hash for Expr {
    fn hash<H: Hasher>(&self, state: &mut H) {
        match self {
            Expr::Assignment(a) => ptr::hash(&**a, state),
            Expr::Binary(a) => ptr::hash(&**a, state),
            Expr::Call(a) => ptr::hash(&**a, state),
            Expr::Grouping(a) => ptr::hash(&**a, state),
            Expr::Literal(a) => ptr::hash(&**a, state),
            Expr::Unary(a) => ptr::hash(&**a, state),
            Expr::Var(a) => ptr::hash(&**a, state),
            Expr::Logical(a) => ptr::hash(&**a, state),
            Expr::Lambda(a) => ptr::hash(&**a, state),
        }
    } 
}

#[derive(Debug, Clone)]
pub struct Assignment {
    pub name: Token,
    pub value: Rc<Expr>,
}

impl Assignment {
    pub fn new(name: Token, value: Rc<Expr>) -> Rc<Self> {
        Rc::new(
            Self { name, value }
        )
    }
}

#[derive(Debug, Clone)]
pub struct Binary {
    pub left: Rc<Expr>,
    pub op: Token,
    pub right: Rc<Expr>,
}

impl Binary {
    pub fn new(left: Rc<Expr>, op: Token, right: Rc<Expr>) -> Rc<Self> {
        Rc::new(
            Self { left, op, right }
        )
    }
}

#[derive(Debug, Clone)]
pub struct Call {
    pub callee: Rc<Expr>,
    pub paren: Token,
    pub args: Vec<Rc<Expr>>,
}

impl Call {
    pub fn new(callee: Rc<Expr>, paren: Token, args: Vec<Rc<Expr>>) -> Rc<Self> {
        Rc::new(
            Self { callee, paren, args }
        )
    }
}

#[derive(Debug, Clone)]
pub struct Grouping {
    pub expr: Rc<Expr>,
}

impl Grouping {
    pub fn new(expr: Rc<Expr>) -> Rc<Self> {
        Rc::new(
            Self { expr }
        )
    }
}

#[derive(Debug, Clone)]
pub struct Literal {
    pub lit: Object,
}

impl Literal {
    pub fn new(lit: Object) -> Rc<Self> {
        Rc::new(
            Self { lit }
        )
    }
}

#[derive(Debug, Clone)]
pub struct Unary {
    pub op: Token,
    pub expr: Rc<Expr>,
}

impl Unary {
    pub fn new(op: Token, expr: Rc<Expr>) -> Rc<Self> {
        Rc::new(
            Self { op, expr }
        )
    }
}

#[derive(Debug, Clone)]
pub struct Var {
    pub name: Token,
}

impl Var {
    pub fn new(name: Token) -> Rc<Self> {
        Rc::new(
            Self { name }
        )
    }
}

#[derive(Debug, Clone)]
pub struct Logical {
    pub left: Rc<Expr>,
    pub op: Token,
    pub right: Rc<Expr>,
}

impl Logical {
    pub fn new(left: Rc<Expr>, op: Token, right: Rc<Expr>) -> Rc<Self> {
        Rc::new(
            Self { left, op, right }
        )
    }
}

#[derive(Debug, Clone)]
pub struct Lambda {
    pub params: Vec<Token>,
    pub body: Vec<Stmt>,
}

impl Lambda {
    pub fn new(params: Vec<Token>, body: Vec<Stmt>) -> Rc<Self> {
        Rc::new(
            Self { params, body }
        )
    }
}
