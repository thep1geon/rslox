use crate::expr;
use crate::token::Token;

pub trait Visitor<T, E> {
    fn expr(&mut self, stmt: &Expression) -> Result<T, E>;
    fn if_stmt(&mut self, stmt: &If) -> Result<T, E>;
    fn print(&mut self, stmt: &Print) -> Result<T, E>;
    fn vardecl(&mut self, stmt: &VarDecl) -> Result<T, E>;
    fn while_stmt(&mut self, stmt: &While) -> Result<T, E>;
    fn block(&mut self, stmt: &Block) -> Result<T, E>;
    fn function(&mut self, stmt: &Function) -> Result<T, E>;
    fn return_stmt(&mut self, stmt: &Return) -> Result<T, E>;
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
}

impl Stmt {
    pub fn accept<T, E>(&self, visitor: &mut dyn Visitor<T, E>) -> Result<T, E> {
        match self {
            Self::Expr(e) => e.accept(visitor),
            Self::If(i) => i.accept(visitor),
            Self::Print(p) => p.accept(visitor),
            Self::VarDecl(v) => v.accept(visitor),
            Self::While(w) => w.accept(visitor),
            Self::Block(b) => b.accept(visitor),
            Self::Function(f) => f.accept(visitor),
            Self::Return(r) => r.accept(visitor),
        }
    }
}

#[derive(Debug, Clone)]
pub struct Expression {
    pub expr: expr::Expr,
}

impl Expression {
    pub fn new(expr: expr::Expr) -> Self {
        Self { expr }
    }

    pub fn accept<T, E>(&self, visitor: &mut dyn Visitor<T, E>) -> Result<T, E> {
        visitor.expr(self)
    }
}

#[derive(Debug, Clone)]
pub struct If {
    pub condition: expr::Expr,
    pub then_branch: Box<Stmt>,
    pub else_branch: Option<Box<Stmt>>,
}

impl If {
    pub fn new(
        condition: expr::Expr,
        then_branch: Box<Stmt>,
        else_branch: Option<Box<Stmt>>,
    ) -> Self {
        Self {
            condition,
            then_branch,
            else_branch,
        }
    }

    pub fn accept<T, E>(&self, visitor: &mut dyn Visitor<T, E>) -> Result<T, E> {
        visitor.if_stmt(self)
    }
}

#[derive(Debug, Clone)]
pub struct Print {
    pub expr: expr::Expr,
}

impl Print {
    pub fn new(expr: expr::Expr) -> Self {
        Self { expr }
    }

    pub fn accept<T, E>(&self, visitor: &mut dyn Visitor<T, E>) -> Result<T, E> {
        visitor.print(self)
    }
}

#[derive(Debug, Clone)]
pub struct VarDecl {
    pub name: Token,
    pub initializer: expr::Expr,
}

impl VarDecl {
    pub fn new(name: Token, initializer: expr::Expr) -> Self {
        Self { name, initializer }
    }

    pub fn accept<T, E>(&self, visitor: &mut dyn Visitor<T, E>) -> Result<T, E> {
        visitor.vardecl(self)
    }
}

#[derive(Debug, Clone)]
pub struct While {
    pub condition: expr::Expr,
    pub statement: Box<Stmt>,
}

impl While {
    pub fn new(condition: expr::Expr, statement: Box<Stmt>) -> Self {
        Self {
            condition,
            statement,
        }
    }

    pub fn accept<T, E>(&self, visitor: &mut dyn Visitor<T, E>) -> Result<T, E> {
        visitor.while_stmt(self)
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

    pub fn accept<T, E>(&self, visitor: &mut dyn Visitor<T, E>) -> Result<T, E> {
        visitor.block(self)
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

    pub fn accept<T, E>(&self, visitor: &mut dyn Visitor<T, E>) -> Result<T, E> {
        visitor.function(self)
    }
}

#[derive(Debug, Clone)]
pub struct Return {
    pub keyword: Token,
    pub value: Option<expr::Expr>,
}

impl Return {
    pub fn new(keyword: Token, value: Option<expr::Expr>) -> Self {
        Self { keyword, value }
    }

    pub fn accept<T, E>(&self, visitor: &mut dyn Visitor<T, E>) -> Result<T, E> {
        visitor.return_stmt(self)
    }
}
