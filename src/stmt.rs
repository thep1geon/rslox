use crate::expr::{self, Expr};
use crate::token::Token;
use std::rc::Rc;

pub trait Visitor<T, E> {
    fn expr(&mut self, stmt: &Expression) -> Result<T, E>;
    fn if_stmt(&mut self, stmt: &If) -> Result<T, E>;
    fn print(&mut self, stmt: &Print) -> Result<T, E>;
    fn vardecl(&mut self, stmt: &VarDecl) -> Result<T, E>;
    fn while_stmt(&mut self, stmt: &While) -> Result<T, E>;
    fn block(&mut self, stmt: &Block) -> Result<T, E>;
    fn function(&mut self, stmt: &Function) -> Result<T, E>;
    fn return_stmt(&mut self, stmt: &Return) -> Result<T, E>;
    fn break_stmt(&mut self, stmt: &Break) -> Result<T, E>;
    fn class_decl(&mut self, stmt: &ClassDecl) -> Result<T, E>;
}

#[derive(Debug, Clone)]
pub enum Stmt {
    Expr(Expression),
    If(If),
    Print(Print),
    VarDecl(VarDecl),
    While(While),
    Block(Block),
    Function(Function),
    Return(Return),
    Break(Break),
    ClassDecl(ClassDecl),
}

impl Stmt {
    pub fn accept<T, E>(&self, visitor: &mut dyn Visitor<T, E>) -> Result<T, E> {
        match self {
            Self::Expr(e) => visitor.expr(e),
            Self::If(i) => visitor.if_stmt(i),
            Self::Print(p) => visitor.print(p),
            Self::VarDecl(v) => visitor.vardecl(v),
            Self::While(w) => visitor.while_stmt(w),
            Self::Block(b) => visitor.block(b),
            Self::Function(f) => visitor.function(f),
            Self::Return(r) => visitor.return_stmt(r),
            Self::Break(b) => visitor.break_stmt(b),
            Self::ClassDecl(c) => visitor.class_decl(c),
        }
    }
}

#[derive(Debug, Clone)]
pub struct Expression {
    pub expr: Rc<Expr>,
}

impl Expression {
    pub fn new(expr: Rc<Expr>) -> Self {
        Self { expr }
    }
}

#[derive(Debug, Clone)]
pub struct If {
    pub condition: Rc<Expr>,
    pub then: Rc<Stmt>,
    pub else_: Option<Rc<Stmt>>,
}

impl If {
    pub fn new(condition: Rc<Expr>, then: Rc<Stmt>, else_: Option<Rc<Stmt>>) -> Self {
        Self {
            condition,
            then,
            else_,
        }
    }
}

#[derive(Debug, Clone)]
pub struct Print {
    pub expr: Rc<Expr>,
}

impl Print {
    pub fn new(expr: Rc<Expr>) -> Self {
        Self { expr }
    }
}

#[derive(Debug, Clone)]
pub struct VarDecl {
    pub name: Token,
    pub initializer: Rc<Expr>,
}

impl VarDecl {
    pub fn new(name: Token, initializer: Rc<Expr>) -> Self {
        Self { name, initializer }
    }
}

#[derive(Debug, Clone)]
pub struct While {
    pub condition: Rc<Expr>,
    pub statement: Rc<Stmt>,
}

impl While {
    pub fn new(condition: Rc<Expr>, statement: Rc<Stmt>) -> Self {
        Self {
            condition,
            statement,
        }
    }
}

#[derive(Debug, Clone)]
pub struct Block {
    pub statements: Vec<Stmt>,
}

impl Block {
    pub fn new(statements: Vec<Stmt>) -> Self {
        Self { statements }
    }
}

#[derive(Debug, Clone)]
pub struct Function {
    pub name: Token,
    pub params: Vec<Token>,
    pub body: Vec<Stmt>,
}

impl Function {
    pub fn new(name: Token, params: Vec<Token>, body: Vec<Stmt>) -> Self {
        Self { name, params, body }
    }
}

#[derive(Debug, Clone)]
pub struct Return {
    pub keyword: Token,
    pub value: Option<Rc<Expr>>,
}

impl Return {
    pub fn new(keyword: Token, value: Option<Rc<Expr>>) -> Self {
        Self { keyword, value }
    }
}

#[derive(Debug, Clone)]
pub struct Break;

impl Break {
    pub fn new() -> Self {
        Self {}
    }
}

#[derive(Debug, Clone)]
pub struct ClassDecl {
    pub name: Token,
    pub superclass: Option<Rc<expr::Var>>,
    pub methods: Vec<Function>,
}

impl ClassDecl {
    pub fn new(name: Token, superclass: Option<Rc<expr::Var>>, methods: Vec<Function>) -> Self {
        Self { name, superclass, methods }
    }
}
