use std::collections::HashMap;
use std::fmt;
use std::rc::Rc;

use crate::expr::{self, Expr}; 
use crate::interpreter::Interpreter;
use crate::stmt::{self, Stmt};
use crate::token::Token;

pub enum ErrorKind {
    LocalVariableInitSelf, 
    VariableAlreadyDefined,
    CannotReturnFromTopLevel,
}

pub struct Error {
    pub kind: ErrorKind,
    pub tok: Token,
}

impl Error {
    pub fn local_variable_init_self(tok: Token) -> Self {
        Self {
            kind: ErrorKind::LocalVariableInitSelf,
            tok,
        }
    }

    pub fn variable_already_defined(tok: Token) -> Self {
        Self {
            kind: ErrorKind::VariableAlreadyDefined,
            tok,
        }
    }

    pub fn cannot_return_from_top_level(tok: Token) -> Self {
        Self {
            kind: ErrorKind::CannotReturnFromTopLevel,
            tok,
        }
    }
}

impl fmt::Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self.kind {
            ErrorKind::LocalVariableInitSelf => write!(f, "LocalVariableInitSelf"),
            ErrorKind::VariableAlreadyDefined => write!(f, "VariableAlreadyDefined"),
            ErrorKind::CannotReturnFromTopLevel => write!(f, "CannotReturnFromTopLevel"),
        }
    }
}

#[derive(Clone, Copy, PartialEq)]
enum FunctionType {
    None,
    Function,
    Lambda,
}

pub struct Resolver<'a> {
    pub interpreter: &'a mut Interpreter,
    pub scopes: Vec<HashMap<String, bool>>,
    current_func: FunctionType,
}

impl<'a> Resolver<'a> {
    pub fn new(interpreter: &'a mut Interpreter) -> Self {
        Self {
            interpreter,
            scopes: vec![],
            current_func: FunctionType::None,
        }
    }

    pub fn resolve_stmts(&mut self, stmts: &Vec<Stmt>) -> Result<(), Error> {
        for stmt in stmts {
            self.resolve_stmt(stmt)?;
        }

        Ok(())
    }

    fn resolve_stmt(&mut self, stmt: &Stmt) -> Result<(), Error> {
        stmt.accept(self)
    }

    fn resolve_expr(&mut self, expr: &Expr) -> Result<(), Error> {
        expr.accept(self)
    }

    fn resolve_local(&mut self, expr: Rc<Expr>, name: &Token) -> Result<(), Error> {
        let scopes = self.scopes.iter().rev().enumerate();
        for (i, scope) in scopes {
            if scope.contains_key(&name.kind.as_string()) {
                self.interpreter.resolve(expr, i+1);
                return Ok(());
            }
        }

        Ok(())
    }

    fn resolve_function(&mut self, stmt: &stmt::Function, func_type: FunctionType) -> Result<(), Error>{
        let enclosing_func = self.current_func;
        self.current_func = func_type;
        self.begin_scope();
        for param in &stmt.params {
            self.declare(param)?;
            self.define(param);
        }

        self.resolve_stmts(&stmt.body)?;
        self.end_scope();
        self.current_func = enclosing_func;
        Ok(())
    }

    fn resolve_lambda(&mut self, expr: &expr::Lambda, func_type: FunctionType) -> Result<(), Error> {
        let enclosing_func = self.current_func;
        self.current_func = func_type;
        self.begin_scope();
        for param in &expr.params {
            self.declare(param)?;
            self.define(param);
        }

        self.resolve_stmts(&expr.body)?;
        self.end_scope();
        self.current_func = enclosing_func;
        Ok(())
    }

    fn begin_scope(&mut self) {
        self.scopes.push(HashMap::new())
    }

    fn end_scope(&mut self) {
        self.scopes.pop();
    }

    fn curr_scope(&self) -> &HashMap<String, bool> {
        self.scopes.last().unwrap()
    }

    fn declare(&mut self, name: &Token) -> ::std::result::Result<(), Error> {
        if self.scopes.is_empty() {
            return Ok(());
        }

        if self.scopes.last().unwrap().contains_key(&name.kind.as_string()) {
            return Err(Error::variable_already_defined(name.clone()));
        }

        self.scopes.last_mut().unwrap().insert(name.kind.as_string(), false);
        Ok(())
    }

    fn define(&mut self, name: &Token) {
        if self.scopes.is_empty() {
            return;
        }

        self.scopes.last_mut().unwrap().insert(name.kind.as_string(), true);
    }
}

impl<'a> stmt::Visitor<(), Error> for Resolver<'a> {
    fn block(&mut self, stmt: &stmt::Block) -> Result<(), Error> {
        self.begin_scope(); 
        self.resolve_stmts(&stmt.statements)?;
        self.end_scope();
        Ok(())
    }

    fn vardecl(&mut self, stmt: &stmt::VarDecl) -> Result<(), Error> {
        self.declare(&stmt.name)?;

        self.resolve_expr(&stmt.initializer)?;

        self.define(&stmt.name);
        Ok(())
    }

    fn function(&mut self, stmt: &stmt::Function) -> Result<(), Error> {
        self.declare(&stmt.name)?;
        self.define(&stmt.name);

        self.resolve_function(stmt, FunctionType::Function)
    }

    fn expr(&mut self, stmt: &stmt::Expression) -> Result<(), Error> {
        self.resolve_expr(&stmt.expr)
    }

    fn if_stmt(&mut self, stmt: &stmt::If) -> Result<(), Error> {
        self.resolve_expr(&stmt.condition)?;
        self.resolve_stmt(&stmt.then)?;

        match &stmt.else_ {
            Some(else_) => self.resolve_stmt(else_),
            None => Ok(())
        }
    }

    fn print(&mut self, stmt: &stmt::Print) -> Result<(), Error> {
        self.resolve_expr(&stmt.expr)
    }

    fn return_stmt(&mut self, stmt: &stmt::Return) -> Result<(), Error> {
        if self.current_func == FunctionType::None {
            return Err(Error::cannot_return_from_top_level(stmt.keyword.clone()));
        }

        match &stmt.value {
            Some(val) => self.resolve_expr(val),
            None => Ok(())
        }
    }

    fn while_stmt(&mut self, stmt: &stmt::While) -> Result<(), Error> {
        self.resolve_expr(&stmt.condition)?;
        self.resolve_stmt(&stmt.statement)
    }

    fn break_stmt(&mut self, _stmt: &stmt::Break) -> Result<(), Error> {
        Ok(())
    }
}

impl<'a> expr::Visitor<(), Error> for Resolver<'a> {
    fn var(&mut self, expr: Rc<expr::Var>) -> Result<(), Error> {
        if !self.scopes.is_empty() { 
            if let Some(var) = self.curr_scope().get(&expr.name.kind.as_string()) {
                if !(*var) {
                    return Err(Error::local_variable_init_self(expr.name.clone()));
                }
            }
        }

        self.resolve_local(Rc::new(Expr::Var(Rc::clone(&expr))), &expr.name)?;

        Ok(())
    }

    fn assignment(&mut self, expr: Rc<expr::Assignment>) -> Result<(), Error> {
        self.resolve_expr(&expr.value)?;
        self.resolve_local(Rc::new(Expr::Assignment(Rc::clone(&expr))), &expr.name)?;
        Ok(())
    }

    fn binary(&mut self, expr: Rc<expr::Binary>) -> Result<(), Error> {
        self.resolve_expr(&expr.left)?;
        self.resolve_expr(&expr.right)
    }

    fn call(&mut self, expr: Rc<expr::Call>) -> Result<(), Error> {
        self.resolve_expr(&expr.callee)?;

        for arg in &expr.args {
            self.resolve_expr(arg)?;
        }

        Ok(())
    }

    fn grouping(&mut self, expr: Rc<expr::Grouping>) -> Result<(), Error> {
        self.resolve_expr(&expr.expr)
    }

    fn literal(&mut self, _expr: Rc<expr::Literal>) -> Result<(), Error> {
        Ok(())
    }

    fn logical(&mut self, expr: Rc<expr::Logical>) -> Result<(), Error> {
        self.resolve_expr(&expr.left)?;
        self.resolve_expr(&expr.right)
    }

    fn unary(&mut self, expr: Rc<expr::Unary>) -> Result<(), Error> {
        self.resolve_expr(&expr.expr)
    }

    fn lambda(&mut self, expr: Rc<expr::Lambda>) -> Result<(), Error> {
        self.resolve_lambda(&expr, FunctionType::Lambda)
    }
}