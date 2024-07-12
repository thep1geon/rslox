use std::fmt;
use std::rc::Rc;

use crate::expr::{Assignment, Binary, Call, Expr, Grouping, Lambda, Literal, Logical, Unary, Var};
use crate::object::Object;
use crate::stmt::{
    Block, Break, Expression, Function, If, Print, Return, Stmt, VarDecl, While,
};
use crate::token::Token;
use crate::token_kind::TokenKind;

pub enum ErrorKind {
    UnexpectedToken,
}

pub struct Error {
    pub kind: ErrorKind,
    pub tok: Token,
}

impl Error {
    pub fn unexpected_token(tok: Token) -> Self {
        Self {
            kind: ErrorKind::UnexpectedToken,
            tok
        }
    }
}

impl fmt::Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self.kind {
            ErrorKind::UnexpectedToken => write!(f, "UnexpectedToken"), 
        }
    } 
}

pub type Result<T> = ::std::result::Result<T, Error>;

pub struct Parser {
    tokens: Vec<Token>,
    current: usize,
}

impl Parser {
    pub fn new(tokens: Vec<Token>) -> Self {
        Self { tokens, current: 0 }
    }

    fn prev(&self) -> Option<&Token> {
        self.tokens.get(self.current - 1)
    }

    fn peek(&self) -> &Token {
        self.tokens.get(self.current).unwrap()
    }

    fn bound(&self) -> bool {
        !matches!(self.peek().kind, TokenKind::Eof)
    }

    fn check(&self, tok: &TokenKind) -> bool {
        if !self.bound() {
            return false;
        }

        return self.peek().kind == *tok;
    }

    fn advance(&mut self) -> &Token {
        if self.bound() {
            self.current += 1;
        }

        self.prev().expect("We just advanced, there should be a previous token")
    }

    fn expect(&mut self, kind: &TokenKind) -> Result<()> {
        if !self.check(kind) {
            self.error(
                format!(
                    "Expected: '{}' but got '{}' instead",
                    kind.as_string(),
                    self.peek().kind.as_string()
                )
                .as_str(),
            );
            return Err(Error::unexpected_token(self.peek().clone()));
        }

        self.advance();
        Ok(())
    }

    fn error(&self, msg: &str) {
        eprintln!("[Line:{}] Error at '{}': {msg}", self.peek().line, self.peek().kind.as_string());
    }

    fn matches(&mut self, toks: &[TokenKind]) -> bool {
        for tok in toks {
            if self.check(tok) {
                self.advance();
                return true;
            }
        }

        false
    }

    fn sync(&mut self) {
        self.advance();

        while self.bound() {
            if self.prev().unwrap().kind == TokenKind::Semicolon {
                return;
            }

            match self.peek().kind {
                TokenKind::Class
                | TokenKind::Fun
                | TokenKind::Var
                | TokenKind::For
                | TokenKind::If
                | TokenKind::While
                | TokenKind::Print
                | TokenKind::Return => return,
                _ => self.advance(),
            };
        }
    }

    pub fn parse(&mut self) -> Result<Vec<Stmt>> {
        let mut statements: Vec<Stmt> = Vec::new();

        while self.bound() {
            statements.push(match self.declaration() {
                Ok(decl) => decl,
                Err(_) => {
                    self.sync();
                    continue;
                }
            });
        }

        Ok(statements)
    }

    fn declaration(&mut self) -> Result<Stmt> {
        if self.matches(&[TokenKind::Var]) {
            return self.var_declaration();
        }

        if self.matches(&[TokenKind::Fun]) {
            return self.function_declaration();
        }

        self.statement()
    }

    fn function_declaration(&mut self) -> Result<Stmt> {
        if !matches!(self.peek().kind, TokenKind::Ident(_)) {
            self.error(
                format!(
                    "Expected function name but got '{}' instead",
                    self.peek().kind.as_string()
                )
                .as_str(),
            );
            return Err(Error::unexpected_token(self.peek().clone()));
        }

        let name = self.advance().clone();

        self.expect(&TokenKind::LParen)?;
        let mut parameters: Vec<Token> = vec![];

        if !self.check(&TokenKind::RParen) {
            loop {
                if parameters.len() >= 255 {
                    self.error("Cannot have more than 255 parameters");
                }

                if !matches!(self.peek().kind, TokenKind::Ident(_)) {
                    self.error("Expected parameter name");
                    return Err(Error::unexpected_token(self.peek().clone()));
                }

                parameters.push(self.advance().clone());

                if !self.matches(&[TokenKind::Comma]) {
                    break;
                }
            }
        }

        self.expect(&TokenKind::RParen)?;

        if !self.check(&TokenKind::LBrace) {
            self.error("Expected '{' before function body");
            return Err(Error::unexpected_token(self.peek().clone()));
        }

        let body = self.block()?;
        Ok(Stmt::Function(Function::new(name, parameters, body)))
    }

    fn var_declaration(&mut self) -> Result<Stmt> {
        if !matches!(self.peek().kind, TokenKind::Ident(_)) {
            self.error(
                format!(
                    "Expected: IDENTIFIER but got '{}' instead",
                    self.peek().kind.as_string()
                )
                .as_str(),
            );
            return Err(Error::unexpected_token(self.peek().clone()));
        }

        let name = self.advance().clone();

        let mut init = Rc::new(Expr::Literal(Literal::new(Object::Nil)));

        if self.matches(&[TokenKind::Eq]) {
            init = self.expression()?;
        }

        self.expect(&TokenKind::Semicolon)?;
        Ok(Stmt::VarDecl(VarDecl::new(name, init)))
    }

    fn statement(&mut self) -> Result<Stmt> {
        match self.peek().kind {
            TokenKind::Print => self.print_statement(),
            TokenKind::LBrace => Ok(Stmt::Block(Block::new(self.block()?))),
            TokenKind::If => self.if_statement(),
            TokenKind::While => self.while_statement(),
            TokenKind::For => self.for_statement(),
            TokenKind::Return => self.return_statement(),
            TokenKind::Break => self.break_statement(),
            _ => self.expression_statement(),
        }
    }

    fn break_statement(&mut self) -> Result<Stmt> {
        self.advance();
        self.expect(&TokenKind::Semicolon)?;
        Ok(Stmt::Break(Break::new()))
    }

    fn return_statement(&mut self) -> Result<Stmt> {
        let keyword = self.advance().clone();

        let value = if self.check(&TokenKind::Semicolon) {
            None
        } else {
            Some(self.expression()?)
        };

        self.expect(&TokenKind::Semicolon)?;

        Ok(Stmt::Return(Return::new(keyword, value)))
    }

    fn for_statement(&mut self) -> Result<Stmt> {
        self.advance();
        self.expect(&TokenKind::LParen)?;

        let initializer: Option<Stmt>;

        if self.matches(&[TokenKind::Var]) {
            initializer = Some(self.var_declaration()?);
        } else if self.matches(&[TokenKind::Semicolon]) {
            initializer = None;
        } else {
            initializer = Some(self.expression_statement()?);
        }

        let mut condition: Option<Rc<Expr>> = if self.check(&TokenKind::RParen) {
            None
        } else {
            Some(self.expression()?)
        };

        self.expect(&TokenKind::Semicolon)?;

        let increment: Option<Rc<Expr>> = if self.check(&TokenKind::RParen) {
            None
        } else {
            Some(self.expression()?)
        };

        self.expect(&TokenKind::RParen)?;

        let mut body = self.statement()?;

        if let Some(inc) = increment {
            body = Stmt::Block(Block::new(vec![body, Stmt::Expr(Expression::new(inc))]));
        }

        if condition.is_none() {
            condition = Some(Rc::new(Expr::Literal(Literal::new(Object::Bool(true)))));
        }

        body = Stmt::While(While::new(condition.unwrap(), Rc::new(body)));

        if let Some(init) = initializer {
            body = Stmt::Block(Block::new(vec![init, body]));
        }

        Ok(body)
    }

    fn while_statement(&mut self) -> Result<Stmt> {
        self.advance();
        self.expect(&TokenKind::LParen)?;
        let expr = self.expression()?;
        self.expect(&TokenKind::RParen)?;

        let stmt = self.statement()?;

        Ok(Stmt::While(While::new(expr, Rc::new(stmt))))
    }

    fn if_statement(&mut self) -> Result<Stmt> {
        self.advance();
        self.expect(&TokenKind::LParen)?;
        let expr = self.expression()?;
        self.expect(&TokenKind::RParen)?;

        let then = Rc::new(self.statement()?);

        let else_ = if self.matches(&[TokenKind::Else]) {
            Some(Rc::new(self.statement()?))
        } else {
            None
        };

        Ok(Stmt::If(If::new(expr, then, else_)))
    }

    fn block(&mut self) -> Result<Vec<Stmt>> {
        self.advance();
        let mut stmts: Vec<Stmt> = Vec::new();

        while !self.check(&TokenKind::RBrace) {
            stmts.push(self.declaration()?);
        }

        self.expect(&TokenKind::RBrace)?;

        Ok(stmts)
    }

    fn print_statement(&mut self) -> Result<Stmt> {
        self.advance();
        let val = self.expression()?;
        self.expect(&TokenKind::Semicolon)?;
        Ok(Stmt::Print(Print::new(val)))
    }

    fn expression_statement(&mut self) -> Result<Stmt> {
        let val = self.expression()?;
        self.expect(&TokenKind::Semicolon)?;
        Ok(Stmt::Expr(Expression::new(val)))
    }

    fn expression(&mut self) -> Result<Rc<Expr>> {
        self.assignment()
    }

    fn assignment(&mut self) -> Result<Rc<Expr>> {
        let expr = self.logical_or()?;

        if self.matches(&[TokenKind::Eq]) {
            let value = self.assignment()?;

            if matches!(expr.as_ref(), Expr::Var(_)) {
                let name = match &expr.as_ref() {
                    Expr::Var(v) => v.name.clone(),
                    _ => unreachable!(),
                };

                return Ok(Rc::new(Expr::Assignment(Assignment::new(name, Rc::clone(&value)))));
            }

            self.error("Invalid assignment target");
        }

        Ok(expr)
    }

    fn logical_or(&mut self) -> Result<Rc<Expr>> {
        let mut expr = self.logical_and()?;

        while self.matches(&[TokenKind::Or]) {
            let op = self.prev().unwrap().clone();
            let right = self.logical_and()?;
            expr = Rc::new(Expr::Logical(Logical::new(Rc::clone(&expr), op, Rc::clone(&right))));
        }

        Ok(expr)
    }

    fn logical_and(&mut self) -> Result<Rc<Expr>> {
        let mut expr = self.equality()?;

        while self.matches(&[TokenKind::And]) {
            let op = self.prev().unwrap().clone();
            let right = self.logical_and()?;
            expr = Rc::new(Expr::Logical(Logical::new(Rc::clone(&expr), op, Rc::clone(&right))));
        }

        Ok(expr)
    }

    fn equality(&mut self) -> Result<Rc<Expr>> {
        let mut expr = self.comparison()?;

        while self.matches(&[TokenKind::BangEq, TokenKind::EqEq]) {
            let op = self.prev().unwrap().clone();
            let right = self.comparison()?;
            expr = Rc::new(Expr::Binary(Binary::new(Rc::clone(&expr), op, Rc::clone(&right))));
        }

        Ok(expr)
    }

    fn comparison(&mut self) -> Result<Rc<Expr>> {
        let mut expr = self.term()?;

        while self.matches(&[TokenKind::Gt, TokenKind::GtEq, TokenKind::Lt, TokenKind::LtEq]) {
            let op = self.prev().unwrap().clone();
            let right = self.term()?;
            expr = Rc::new(Expr::Binary(Binary::new(Rc::clone(&expr), op, Rc::clone(&right))));
        }

        Ok(expr)
    }

    fn term(&mut self) -> Result<Rc<Expr>> {
        let mut expr = self.factor()?;

        while self.matches(&[TokenKind::Plus, TokenKind::Minus]) {
            let op = self.prev().unwrap().clone();
            let right = self.factor()?;
            expr = Rc::new(Expr::Binary(Binary::new(Rc::clone(&expr), op, Rc::clone(&right))));
        }

        Ok(expr)
    }

    fn factor(&mut self) -> Result<Rc<Expr>> {
        let mut expr = self.unary()?;

        while self.matches(&[TokenKind::Slash, TokenKind::Star]) {
            let op = self.prev().unwrap().clone();
            let right = self.unary()?;
            expr = Rc::new(Expr::Binary(Binary::new(Rc::clone(&expr), op, Rc::clone(&right))));
        }

        Ok(expr)
    }

    fn unary(&mut self) -> Result<Rc<Expr>> {
        if self.matches(&[TokenKind::Bang, TokenKind::Minus]) {
            let op = self.prev().unwrap().clone();
            let right = self.unary()?;
            return Ok(Rc::new(Expr::Unary(Unary::new(op, Rc::clone(&right)))));
        }

        self.call()
    }

    fn call(&mut self) -> Result<Rc<Expr>> {
        let mut expr = self.primary()?;

        loop {
            if !self.matches(&[TokenKind::LParen]) {
                break;
            }

            expr = self.finish_call(expr)?;
        }

        Ok(expr)
    }

    fn finish_call(&mut self, callee: Rc<Expr>) -> Result<Rc<Expr>> {
        let mut args: Vec<Rc<Expr>> = vec![];
        if !self.check(&TokenKind::RParen) {
            loop {
                if args.len() >= 255 {
                    self.error("Cannot have more than 255 arguments");
                }

                args.push(self.expression()?);
                if !self.matches(&[TokenKind::Comma]) {
                    break;
                }
            }
        }

        self.expect(&TokenKind::RParen)?;
        let paren = self.prev().unwrap().clone();

        Ok(Rc::new(Expr::Call(Call::new(callee, paren, args))))
    }

    fn primary(&mut self) -> Result<Rc<Expr>> {
        match &self.advance().kind {
            TokenKind::Fun => self.lambda_expression(),
            TokenKind::Nil => Ok(Rc::new(Expr::Literal(Literal::new(Object::Nil)))),

            TokenKind::Ident(_) => Ok(Rc::new(Expr::Var(Var::new(self.prev().unwrap().clone())))),

            TokenKind::True => Ok(Rc::new(Expr::Literal(Literal::new(Object::Bool(true))))),

            TokenKind::False => Ok(Rc::new(Expr::Literal(Literal::new(Object::Bool(false))))),

            TokenKind::Number(n) => Ok(Rc::new(Expr::Literal(Literal::new(Object::Number(*n))))),

            TokenKind::Str(s) => Ok(Rc::new(Expr::Literal(Literal::new(Object::Str(s.clone()))))),

            TokenKind::LParen => {
                let expr = self.expression()?;
                self.expect(&TokenKind::RParen)?;
                Ok(Rc::new(Expr::Grouping(Grouping::new(expr))))
            }
            _ => {
                self.error("Unknown Token");
                Err(Error::unexpected_token(self.peek().clone()))
            }
        }
    }

    fn lambda_expression(&mut self) -> Result<Rc<Expr>> {
        self.expect(&TokenKind::LParen)?;
        let mut parameters: Vec<Token> = vec![];

        if !self.check(&TokenKind::RParen) {
            loop {
                if parameters.len() >= 255 {
                    self.error("Cannot have more than 255 parameters");
                }

                if !matches!(self.peek().kind, TokenKind::Ident(_)) {
                    self.error("Expected parameter name");
                    return Err(Error::unexpected_token(self.peek().clone()));
                }

                parameters.push(self.advance().clone());

                if !self.matches(&[TokenKind::Comma]) {
                    break;
                }
            }
        }

        self.expect(&TokenKind::RParen)?;

        if !self.check(&TokenKind::LBrace) {
            self.error("Expected '{' before function body");
            return Err(Error::unexpected_token(self.peek().clone()));
        }

        let body = self.block()?;

        Ok(Rc::new(Expr::Lambda(Lambda::new(parameters, body))))
    }
}
