use crate::token::Token;
use std::collections::HashMap;

#[derive(Debug)]
pub enum Error {
    ByteToStringConversion,
    NumberParsing,
    UnknownCharacter,
    UnterminatedString,
}

pub type Result<T> = std::result::Result<T, Error>;

pub struct Lexer<'a> {
    src: &'a str,
    tokens: Vec<Token>,

    start: usize,
    current: usize,
    line: usize,

    keywords: HashMap<String, Token>,
}

fn is_alpha(c: char) -> bool {
    matches!(c, 'a'..='z' | 'A'..='Z' | '_')
}

fn is_alnum(c: char) -> bool {
    is_alpha(c) || c.is_ascii_digit()
}

impl<'a> Lexer<'a> {
    #[must_use]
    pub fn new(src: &'a str) -> Self {
        Self {
            src,
            tokens: vec![],
            start: 0,
            current: 0,
            line: 1,
            keywords: HashMap::new(),
        }
    }

    fn init_keywords(&mut self) {
        self.keywords.insert("and".to_string(), Token::And);
        self.keywords.insert("class".to_string(), Token::Class);
        self.keywords.insert("else".to_string(), Token::Else);
        self.keywords.insert("false".to_string(), Token::False);
        self.keywords.insert("for".to_string(), Token::For);
        self.keywords.insert("fun".to_string(), Token::Fun);
        self.keywords.insert("if".to_string(), Token::If);
        self.keywords.insert("nil".to_string(), Token::Nil);
        self.keywords.insert("or".to_string(), Token::Or);
        self.keywords.insert("print".to_string(), Token::Print);
        self.keywords.insert("return".to_string(), Token::Return);
        self.keywords.insert("super".to_string(), Token::Super);
        self.keywords.insert("this".to_string(), Token::This);
        self.keywords.insert("true".to_string(), Token::True);
        self.keywords.insert("var".to_string(), Token::Var);
        self.keywords.insert("while".to_string(), Token::While);
    }

    fn peek(&self) -> char {
        self.peekn(0)
    }

    fn peekn(&self, n: usize) -> char {
        if self.current + n >= self.src.len() {
            return '\0';
        }

        self.src.as_bytes()[self.current + n] as char
    }

    fn bound(&self) -> bool {
        self.current < self.src.len()
    }

    fn advance(&mut self) -> char {
        let c: char = self.peek();
        self.current += 1;
        c
    }

    fn add(&mut self, tok: Token) {
        self.tokens.push(tok);
    }

    fn consume_comment(&mut self) {
        while self.peek() != '\n' && self.bound() {
            _ = self.advance();
        }
    }

    fn expect(&mut self, c: char) -> bool {
        if !self.bound() {
            return false;
        }

        if self.peek() != c {
            return false;
        }

        self.current += 1;
        true
    }

    fn scan_token(&mut self) -> Result<()> {
        let c = self.advance();

        match c as u8 {
            b' ' | b'\r' | b'\t' => (),
            b'\n' => self.line += 1,
            b'(' => self.add(Token::LParen),
            b')' => self.add(Token::RParen),
            b'{' => self.add(Token::LBrace),
            b'}' => self.add(Token::RBrace),
            b',' => self.add(Token::Comma),
            b'.' => self.add(Token::Dot),
            b'-' => self.add(Token::Minus),
            b'+' => self.add(Token::Plus),

            b'/' => {
                if self.expect('/') {
                    self.consume_comment();
                } else {
                    self.add(Token::Slash);
                }
            }
            b';' => self.add(Token::Semicolon),
            b'*' => self.add(Token::Star),

            b'!' => {
                if self.expect('=') {
                    self.add(Token::BangEq);
                } else {
                    self.add(Token::Bang);
                }
            }

            b'=' => {
                if self.expect('=') {
                    self.add(Token::EqEq);
                } else {
                    self.add(Token::Eq);
                }
            }

            b'<' => {
                if self.expect('=') {
                    self.add(Token::LtEq);
                } else {
                    self.add(Token::Lt);
                }
            }

            b'>' => {
                if self.expect('=') {
                    self.add(Token::GtEq);
                } else {
                    self.add(Token::Gt);
                }
            }

            b'"' => self.scan_string()?,

            b'0'..=b'9' => self.scan_number()?,

            b'a'..=b'z' | b'A'..=b'Z' | b'_' => self.scan_ident()?,

            _ => return Err(Error::UnknownCharacter),
        }

        Ok(())
    }

    fn scan_string(&mut self) -> Result<()> {
        while self.peek() != '"' && self.bound() {
            if self.peek() == '\n' {
                self.line += 1;
            }

            self.advance();
        }

        if !self.bound() {
            return Err(Error::UnterminatedString);
        }

        self.advance();

        let bytes = &self.src.as_bytes()[(self.start + 1)..(self.current - 1)];

        let str = match std::str::from_utf8(bytes) {
            Ok(s) => s,
            Err(_) => return Err(Error::ByteToStringConversion),
        }
        .to_string();

        self.add(Token::String(str));

        Ok(())
    }

    fn scan_number(&mut self) -> Result<()> {
        while self.peek().is_ascii_digit() {
            self.advance();
        }

        if self.peek() == '.' && self.peekn(1).is_ascii_digit() {
            self.advance();

            while self.peek().is_ascii_digit() {
                self.advance();
            }
        }

        let bytes = &self.src.as_bytes()[(self.start)..(self.current)];

        let str = match std::str::from_utf8(bytes) {
            Ok(s) => s,
            Err(_) => return Err(Error::ByteToStringConversion),
        }
        .to_string();

        self.add(Token::Number(match str.parse::<f64>() {
            Ok(num) => num,
            Err(_) => return Err(Error::NumberParsing),
        }));

        Ok(())
    }

    fn scan_ident(&mut self) -> Result<()> {
        while is_alnum(self.peek()) {
            self.advance();
        }

        let bytes = &self.src.as_bytes()[(self.start)..(self.current)];

        let str = match std::str::from_utf8(bytes) {
            Ok(s) => s,
            Err(_) => return Err(Error::ByteToStringConversion),
        }
        .to_string();

        match self.keywords.get(&str) {
            Some(tok) => self.add(tok.clone()),
            None => self.add(Token::Ident(str)),
        }

        Ok(())
    }

    /// # Errors
    ///
    /// This function would return an error if anything major went wrong
    /// with lexing the string
    pub fn scan_tokens(&mut self) -> Result<&Vec<Token>> {
        self.init_keywords();
        while self.bound() {
            self.start = self.current;
            self.scan_token()?;
        }

        self.tokens.push(Token::Eof);
        Ok(&self.tokens)
    }
}
