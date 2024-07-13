use crate::token::Token;
use crate::token_kind::TokenKind::*;
use std::fmt;
use std::collections::HashMap;

pub enum ErrorKind {
    ByteToStringConversion,
    NumberParsing,
    UnknownCharacter,
    UnterminatedString,
}

pub struct Error {
    pub kind: ErrorKind,
    pub line: usize,
}

impl Error {
    pub fn byte_to_string_conversion(line: usize) -> Self {
        Self { 
            kind: ErrorKind::ByteToStringConversion, 
            line, 
        }
    }

    pub fn number_parsing(line: usize) -> Self {
        Self { 
            kind: ErrorKind::NumberParsing, 
            line, 
        }
    }

    pub fn unknown_character(line: usize) -> Self {
        Self { 
            kind: ErrorKind::UnknownCharacter, 
            line, 
        }
    }

    pub fn unterminated_string(line: usize) -> Self {
        Self {
            kind: ErrorKind::UnterminatedString,
            line,
        }
    }
}

impl fmt::Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self.kind {
            ErrorKind::ByteToStringConversion => write!(f, "ByteToStringConversion"),
            ErrorKind::NumberParsing => write!(f, "NumberParsing"),
            ErrorKind::UnknownCharacter => write!(f, "UnknownCharacter"),
            ErrorKind::UnterminatedString => write!(f, "UnterminatedString"),
        }
    }
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
        self.keywords.insert("and".to_string(), Token::new(And, 0));
        self.keywords.insert("break".to_string(), Token::new(Break, 0));
        self.keywords.insert("class".to_string(), Token::new(Class, 0));
        self.keywords.insert("else".to_string(), Token::new(Else, 0));
        self.keywords.insert("false".to_string(), Token::new(False, 0));
        self.keywords.insert("for".to_string(), Token::new(For, 0));
        self.keywords.insert("fun".to_string(), Token::new(Fun, 0));
        self.keywords.insert("if".to_string(), Token::new(If, 0));
        self.keywords.insert("nil".to_string(), Token::new(Nil, 0));
        self.keywords.insert("or".to_string(), Token::new(Or, 0));
        self.keywords.insert("print".to_string(), Token::new(Print, 0));
        self.keywords.insert("return".to_string(), Token::new(Return, 0));
        self.keywords.insert("super".to_string(), Token::new(Super, 0));
        self.keywords.insert("this".to_string(), Token::new(This, 0));
        self.keywords.insert("true".to_string(), Token::new(True, 0));
        self.keywords.insert("var".to_string(), Token::new(Var, 0));
        self.keywords.insert("while".to_string(), Token::new(While, 0));
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
            b'(' => self.add(Token::new(LParen, self.line)),
            b')' => self.add(Token::new(RParen, self.line)),
            b'{' => self.add(Token::new(LBrace, self.line)),
            b'}' => self.add(Token::new(RBrace, self.line)),
            b',' => self.add(Token::new(Comma, self.line)),
            b'.' => self.add(Token::new(Dot, self.line)),
            b'-' => self.add(Token::new(Minus, self.line)),
            b'+' => self.add(Token::new(Plus, self.line)),

            b'/' => {
                if self.expect('/') {
                    self.consume_comment();
                } else {
                    self.add(Token::new(Slash, self.line));
                }
            }
            b';' => self.add(Token::new(Semicolon, self.line)),
            b'*' => self.add(Token::new(Star, self.line)),

            b'!' => {
                if self.expect('=') {
                    self.add(Token::new(BangEq, self.line));
                } else {
                    self.add(Token::new(Bang, self.line));
                }
            }

            b'=' => {
                if self.expect('=') {
                    self.add(Token::new(EqEq, self.line));
                } else {
                    self.add(Token::new(Eq, self.line));
                }
            }

            b'<' => {
                if self.expect('=') {
                    self.add(Token::new(LtEq, self.line));
                } else {
                    self.add(Token::new(Lt, self.line));
                }
            }

            b'>' => {
                if self.expect('=') {
                    self.add(Token::new(GtEq, self.line));
                } else {
                    self.add(Token::new(Gt, self.line));
                }
            }

            b'"' => self.scan_string()?,

            b'0'..=b'9' => self.scan_number()?,

            b'a'..=b'z' | b'A'..=b'Z' | b'_' => self.scan_ident()?,

            _ => return Err(Error::unknown_character(self.line)),
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
            return Err(Error::unterminated_string(self.line));
        }

        self.advance();

        let bytes = &self.src.as_bytes()[(self.start + 1)..(self.current - 1)];

        let str = match std::str::from_utf8(bytes) {
            Ok(s) => s,
            Err(_) => return Err(Error::byte_to_string_conversion(self.line)),
        }
        .to_string();

        self.add(Token::new(Str(str), self.line));

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
            Err(_) => return Err(Error::byte_to_string_conversion(self.line)),
        }
        .to_string();

        self.add(Token::new(
            Number(match str.parse::<f64>() {
                Ok(num) => num,
                Err(_) => return Err(Error::number_parsing(self.line)),
            }),
            self.line,
        ));

        Ok(())
    }

    fn scan_ident(&mut self) -> Result<()> {
        while is_alnum(self.peek()) {
            self.advance();
        }

        let bytes = &self.src.as_bytes()[(self.start)..(self.current)];

        let str = match std::str::from_utf8(bytes) {
            Ok(s) => s,
            Err(_) => return Err(Error::byte_to_string_conversion(self.line)),
        }
        .to_string();

        match self.keywords.get(&str) {
            Some(tok) => self.add(tok.clone()),
            None => self.add(Token::new(Ident(str), self.line)),
        }

        Ok(())
    }

    pub fn scan_tokens(&mut self) -> Result<&Vec<Token>> {
        self.init_keywords();
        while self.bound() {
            self.start = self.current;
            self.scan_token()?;
        }

        self.tokens.push(Token::new(Eof, self.line));
        Ok(&self.tokens)
    }
}
