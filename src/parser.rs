use crate::expr::{Assignment, Binary, Call, Expr, Grouping, Literal, Logical, Unary, Var};
use crate::object::Object;
use crate::stmt::{Block, Expression, Function, If, Print, Return, Stmt, VarDecl, While};
use crate::token::Token;

#[derive(Debug)]
pub enum Error {
    UnexpectedToken,
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

    fn prev(&self) -> &Token {
        if self.current == 0 {
            return &Token::Eof;
        }
        self.tokens.get(self.current - 1).unwrap()
    }

    fn peek(&self) -> &Token {
        self.tokens.get(self.current).unwrap()
    }

    fn bound(&self) -> bool {
        !matches!(*self.peek(), Token::Eof)
    }

    fn check(&self, tok: &Token) -> bool {
        if !self.bound() {
            return false;
        }

        return self.peek() == tok;
    }

    fn advance(&mut self) -> &Token {
        if self.bound() {
            self.current += 1;
        }

        self.prev()
    }

    fn expect(&mut self, tok: &Token) -> Result<()> {
        if !self.check(tok) {
            self.error(
                format!(
                    "Expected: '{}' but got '{}' instead",
                    tok.as_string(),
                    self.peek().as_string()
                )
                .as_str(),
            );
            return Err(Error::UnexpectedToken);
        }

        self.advance();
        Ok(())
    }

    fn error(&self, msg: &str) {
        eprintln!("Error at '{}': {msg}", self.peek().as_string());
    }

    fn matches(&mut self, toks: &[Token]) -> bool {
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
            if *self.prev() == Token::Semicolon {
                return;
            }

            match self.peek() {
                Token::Class
                | Token::Fun
                | Token::Var
                | Token::For
                | Token::If
                | Token::While
                | Token::Print
                | Token::Return => return,
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
        if self.matches(&[Token::Var]) {
            return self.var_declaration();
        }

        if self.matches(&[Token::Fun]) {
            return self.function_declaration();
        }

        self.statement()
    }

    fn function_declaration(&mut self) -> Result<Stmt> {
        if !matches!(self.peek(), Token::Ident(_)) {
            self.error(
                format!(
                    "Expected function name but got '{}' instead",
                    self.peek().as_string()
                )
                .as_str(),
            );
            return Err(Error::UnexpectedToken);
        }

        let name = self.advance().clone();

        self.expect(&Token::LParen)?;
        let mut parameters: Vec<Token> = vec![];

        if !self.check(&Token::RParen) {
            loop {
                if parameters.len() >= 255 {
                    self.error("Cannot have more than 255 parameters");
                }

                if !matches!(self.peek(), Token::Ident(_)) {
                    self.error(format!("Expected parameter name").as_str());
                    return Err(Error::UnexpectedToken);
                }

                parameters.push(self.advance().clone());

                if !self.matches(&[Token::Comma]) {
                    break;
                }
            }
        }

        self.expect(&Token::RParen)?;

        if !self.check(&Token::LBrace) {
            self.error(format!("Expected '{{' before function body").as_str());
            return Err(Error::UnexpectedToken);
        }

        let body = self.block()?;
        Ok(Stmt::Function(Function::new(name, parameters, body)))
    }

    fn var_declaration(&mut self) -> Result<Stmt> {
        if !matches!(self.peek(), Token::Ident(_)) {
            self.error(
                format!(
                    "Expected: IDENTIFIER but got '{}' instead",
                    self.peek().as_string()
                )
                .as_str(),
            );
            return Err(Error::UnexpectedToken);
        }

        let name = self.advance().clone();

        let mut init = Expr::Literal(Literal::new(Object::Nil));

        if self.matches(&[Token::Eq]) {
            init = self.expression()?;
        }

        self.expect(&Token::Semicolon)?;
        Ok(Stmt::VarDecl(VarDecl::new(name, init)))
    }

    fn statement(&mut self) -> Result<Stmt> {
        match self.peek() {
            Token::Print => self.print_statement(),
            Token::LBrace => Ok(Stmt::Block(Block::new(self.block()?))),
            Token::If => self.if_statement(),
            Token::While => self.while_statement(),
            Token::For => self.for_statement(),
            Token::Return => self.return_statement(),
            _ => self.expression_statement(),
        }
    }

    fn return_statement(&mut self) -> Result<Stmt> {
        let keyword = self.advance().clone();
        
        let mut value: Option<Expr> = None;
        if !self.check(&Token::Semicolon) {
            value = Some(self.expression()?);
        }

        self.expect(&Token::Semicolon)?;

        Ok(Stmt::Return(Return::new(keyword, value)))
    }

    fn for_statement(&mut self) -> Result<Stmt> {
        self.advance();
        self.expect(&Token::LParen)?;

        let initializer: Option<Stmt>;

        if self.matches(&[Token::Var]) {
            initializer = Some(self.var_declaration()?);
        } else if self.matches(&[Token::Semicolon]) {
            initializer = None;
        } else {
            initializer = Some(self.expression_statement()?);
        }

        let mut condition: Option<Expr> = None;
        if !self.check(&Token::Semicolon) {
            condition = Some(self.expression()?);
        }

        self.expect(&Token::Semicolon)?;

        let mut increment: Option<Expr> = None;
        if !self.check(&Token::RParen) {
            increment = Some(self.expression()?);
        }

        self.expect(&Token::RParen)?;

        let mut body = self.statement()?;

        if let Some(inc) = increment {
            body = Stmt::Block(Block::new(vec![body, Stmt::Expr(Expression::new(inc))]));
        }

        if condition.is_none() {
            condition = Some(Expr::Literal(Literal::new(Object::Bool(true))));
        }

        body = Stmt::While(While::new(condition.unwrap(), Box::new(body)));

        if let Some(init) = initializer {
            body = Stmt::Block(Block::new(vec![init, body]));
        }

        Ok(body)
    }

    fn while_statement(&mut self) -> Result<Stmt> {
        self.advance();
        self.expect(&Token::LParen)?;
        let expr = self.expression()?;
        self.expect(&Token::RParen)?;

        let stmt = self.statement()?;

        Ok(Stmt::While(While::new(expr, Box::new(stmt))))
    }

    fn if_statement(&mut self) -> Result<Stmt> {
        self.advance();
        self.expect(&Token::LParen)?;
        let expr = self.expression()?;
        self.expect(&Token::RParen)?;

        let then_branch = Box::new(self.statement()?);
        let mut else_branch: Option<Box<Stmt>> = None;
        if self.matches(&[Token::Else]) {
            else_branch = Some(Box::new(self.statement()?));
        }

        Ok(Stmt::If(If::new(expr, then_branch, else_branch)))
    }

    fn block(&mut self) -> Result<Vec<Stmt>> {
        self.advance();
        let mut stmts: Vec<Stmt> = Vec::new();

        while !self.check(&Token::RBrace) {
            stmts.push(self.declaration()?);
        }

        self.expect(&Token::RBrace)?;

        Ok(stmts)
    }

    fn print_statement(&mut self) -> Result<Stmt> {
        self.advance();
        let val = self.expression()?;
        self.expect(&Token::Semicolon)?;
        Ok(Stmt::Print(Print::new(val)))
    }

    fn expression_statement(&mut self) -> Result<Stmt> {
        let val = self.expression()?;
        self.expect(&Token::Semicolon)?;
        Ok(Stmt::Expr(Expression::new(val)))
    }

    fn expression(&mut self) -> Result<Expr> {
        self.assignment()
    }

    fn assignment(&mut self) -> Result<Expr> {
        let expr = self.logical_or()?;

        if self.matches(&[Token::Eq]) {
            let value = self.assignment()?;

            if matches!(expr, Expr::Var(_)) {
                let name = match &expr {
                    Expr::Var(v) => v.name.clone(),
                    _ => unreachable!(),
                };

                return Ok(Expr::Assignment(Assignment::new(name, Box::new(value))));
            }

            self.error("Invalid assignment target");
        }

        Ok(expr)
    }

    fn logical_or(&mut self) -> Result<Expr> {
        let mut expr = self.logical_and()?;

        while self.matches(&[Token::Or]) {
            let op = self.prev().clone();
            let right = self.logical_and()?;
            expr = Expr::Logical(Logical::new(Box::new(expr), op, Box::new(right)));
        }

        Ok(expr)
    }

    fn logical_and(&mut self) -> Result<Expr> {
        let mut expr = self.equality()?;

        while self.matches(&[Token::And]) {
            let op = self.prev().clone();
            let right = self.logical_and()?;
            expr = Expr::Logical(Logical::new(Box::new(expr), op, Box::new(right)));
        }

        Ok(expr)
    }

    fn equality(&mut self) -> Result<Expr> {
        let mut expr = self.comparison()?;

        while self.matches(&[Token::BangEq, Token::EqEq]) {
            let operator = self.prev().clone();
            let right = self.comparison()?;
            expr = Expr::Binary(Binary::new(Box::new(expr), operator, Box::new(right)));
        }

        Ok(expr)
    }

    fn comparison(&mut self) -> Result<Expr> {
        let mut expr = self.term()?;

        while self.matches(&[Token::Gt, Token::GtEq, Token::Lt, Token::LtEq]) {
            let op = self.prev().clone();
            let right = self.term()?;
            expr = Expr::Binary(Binary::new(Box::new(expr), op, Box::new(right)));
        }

        Ok(expr)
    }

    fn term(&mut self) -> Result<Expr> {
        let mut expr = self.factor()?;

        while self.matches(&[Token::Plus, Token::Minus]) {
            let op = self.prev().clone();
            let right = self.factor()?;
            expr = Expr::Binary(Binary::new(Box::new(expr), op, Box::new(right)));
        }

        Ok(expr)
    }

    fn factor(&mut self) -> Result<Expr> {
        let mut expr = self.unary()?;

        while self.matches(&[Token::Slash, Token::Star]) {
            let op = self.prev().clone();
            let right = self.unary()?;
            expr = Expr::Binary(Binary::new(Box::new(expr), op, Box::new(right)));
        }

        Ok(expr)
    }

    fn unary(&mut self) -> Result<Expr> {
        if self.matches(&[Token::Bang, Token::Minus]) {
            let op = self.prev().clone();
            let right = self.unary()?;
            return Ok(Expr::Unary(Unary::new(op, Box::new(right))));
        }

        self.call()
    }

    fn call(&mut self) -> Result<Expr> {
        let mut expr = self.primary()?;

        loop {
            if !self.matches(&[Token::LParen]) {
                break;
            }

            expr = self.finish_call(expr)?;
        }

        Ok(expr)
    }

    fn finish_call(&mut self, callee: Expr) -> Result<Expr> {
        let mut args: Vec<Expr> = vec![];
        if !self.check(&Token::RParen) {
            loop {
                if args.len() >= 255 {
                    self.error("Cannot have more than 255 arguments");
                }

                args.push(self.expression()?);
                if !self.matches(&[Token::Comma]) {
                    break;
                }
            }
        }

        self.expect(&Token::RParen)?;

        Ok(Expr::Call(Call::new(Box::new(callee), args)))
    }

    fn primary(&mut self) -> Result<Expr> {
        match self.advance() {
            Token::Nil => Ok(Expr::Literal(Literal::new(Object::Nil))),
            Token::Ident(_) => Ok(Expr::Var(Var::new(self.prev().clone()))),
            Token::True => Ok(Expr::Literal(Literal::new(Object::Bool(true)))),
            Token::False => Ok(Expr::Literal(Literal::new(Object::Bool(false)))),
            Token::Number(n) => Ok(Expr::Literal(Literal::new(Object::Number(*n)))),
            Token::String(s) => Ok(Expr::Literal(Literal::new(Object::Str(s.clone())))),
            Token::LParen => {
                let expr = self.expression()?;
                self.expect(&Token::RParen)?;
                Ok(Expr::Grouping(Grouping::new(Box::new(expr))))
            }
            _ => {
                self.error("Unknown Token");
                Err(Error::UnexpectedToken)
            }
        }
    }
}
