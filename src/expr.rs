use crate::object::Object;
use crate::stmt::Function;
use crate::token::Token;

use std::result::Result;

pub trait Visitor<T, E> {
    fn assignment(&mut self, expr: &Assignment) -> Result<T, E>;
    fn binary(&mut self, expr: &Binary) -> Result<T, E>;
    fn call(&mut self, expr: &Call) -> Result<T, E>;
    fn grouping(&mut self, expr: &Grouping) -> Result<T, E>;
    fn literal(&mut self, expr: &Literal) -> Result<T, E>;
    fn unary(&mut self, expr: &Unary) -> Result<T, E>;
    fn var(&mut self, expr: &Var) -> Result<T, E>;
    fn logical(&mut self, expr: &Logical) -> Result<T, E>;
}

#[derive(Debug, Clone)]
pub enum Expr {
    Assignment(Assignment),
    Binary(Binary),
    Call(Call),
    Unary(Unary),
    Literal(Literal),
    Var(Var),
    Grouping(Grouping),
    Logical(Logical),
    // LambdaFunc(Lambda),
}

impl Expr {
    pub fn accept<T, E>(&self, visitor: &mut dyn Visitor<T, E>) -> Result<T, E> {
        match self {
            Expr::Assignment(a) => a.accept(visitor),
            Expr::Binary(b) => b.accept(visitor),
            Expr::Call(c) => c.accept(visitor),
            Expr::Grouping(g) => g.accept(visitor),
            Expr::Literal(l) => l.accept(visitor),
            Expr::Unary(u) => u.accept(visitor),
            Expr::Var(v) => v.accept(visitor),
            Expr::Logical(l) => l.accept(visitor),
        }
    }
}

#[derive(Debug, Clone)]
pub struct Assignment {
    pub name: Token,
    pub value: Box<Expr>,
}

impl Assignment {
    pub fn new(name: Token, value: Box<Expr>) -> Self {
        Self { name, value }
    }

    fn accept<T, E>(&self, visitor: &mut dyn Visitor<T, E>) -> Result<T, E> {
        visitor.assignment(self)
    }
}

#[derive(Debug, Clone)]
pub struct Binary {
    pub left: Box<Expr>,
    pub op: Token,
    pub right: Box<Expr>,
}

impl Binary {
    pub fn new(left: Box<Expr>, op: Token, right: Box<Expr>) -> Self {
        Self { left, op, right }
    }

    fn accept<T, E>(&self, visitor: &mut dyn Visitor<T, E>) -> Result<T, E> {
        visitor.binary(self)
    }
}

#[derive(Debug, Clone)]
pub struct Call {
    pub callee: Box<Expr>,
    pub args: Vec<Expr>,
}

impl Call {
    pub fn new(callee: Box<Expr>, args: Vec<Expr>) -> Self {
        Self { callee, args }
    }

    fn accept<T, E>(&self, visitor: &mut dyn Visitor<T, E>) -> Result<T, E> {
        visitor.call(self)
    }
}

#[derive(Debug, Clone)]
pub struct Grouping {
    pub expr: Box<Expr>,
}

impl Grouping {
    pub fn new(expr: Box<Expr>) -> Self {
        Self { expr }
    }

    fn accept<T, E>(&self, visitor: &mut dyn Visitor<T, E>) -> Result<T, E> {
        visitor.grouping(self)
    }
}

#[derive(Debug, Clone)]
pub struct Literal {
    pub lit: Object,
}

impl Literal {
    pub fn new(literal: Object) -> Self {
        Self { lit: literal }
    }

    fn accept<T, E>(&self, visitor: &mut dyn Visitor<T, E>) -> Result<T, E> {
        visitor.literal(self)
    }
}

#[derive(Debug, Clone)]
pub struct Unary {
    pub op: Token,
    pub expr: Box<Expr>,
}

impl Unary {
    pub fn new(op: Token, expr: Box<Expr>) -> Self {
        Self { op, expr }
    }

    fn accept<T, E>(&self, visitor: &mut dyn Visitor<T, E>) -> Result<T, E> {
        visitor.unary(self)
    }
}

#[derive(Debug, Clone)]
pub struct Var {
    pub name: Token,
}

impl Var {
    pub fn new(name: Token) -> Self {
        Self { name }
    }

    fn accept<T, E>(&self, visitor: &mut dyn Visitor<T, E>) -> Result<T, E> {
        visitor.var(self)
    }
}

#[derive(Debug, Clone)]
pub struct Logical {
    pub left: Box<Expr>,
    pub op: Token,
    pub right: Box<Expr>,
}

impl Logical {
    pub fn new(left: Box<Expr>, op: Token, right: Box<Expr>) -> Self {
        Self { left, op, right }
    }

    fn accept<T, E>(&self, visitor: &mut dyn Visitor<T, E>) -> Result<T, E> {
        visitor.logical(self)
    }
}
