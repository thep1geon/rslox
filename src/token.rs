#[derive(Debug, PartialEq, Clone)]
pub enum Token {
    // Singluar characters
    LParen,
    RParen,
    LBrace,
    RBrace,
    Comma,
    Dot,
    Minus,
    Plus,
    Semicolon,
    Slash,
    Star,

    // One or more characters
    Bang,
    BangEq,
    Eq,
    EqEq,
    Gt,
    GtEq,
    Lt,
    LtEq,

    // Literals
    Ident(String),
    String(String),
    Number(f64),

    // Keywords
    And,
    Class,
    Else,
    False,
    Fun,
    For,
    If,
    Nil,
    Or,
    Print,
    Return,
    Super,
    This,
    True,
    Var,
    While,

    Eof,
}

impl Token {
    pub fn as_string(&self) -> String {
        use self::Token::*;
        match self {
            LParen => "(".to_string(),
            RParen => ")".to_string(),
            LBrace => "{".to_string(),
            RBrace => "}".to_string(),
            Comma => ",".to_string(),
            Dot => ".".to_string(),
            Minus => "-".to_string(),
            Plus => "+".to_string(),
            Semicolon => ";".to_string(),
            Slash => "/".to_string(),
            Star => "*".to_string(),
            Bang => "!".to_string(),
            BangEq => "!=".to_string(),
            Eq => "=".to_string(),
            EqEq => "==".to_string(),
            Gt => ">".to_string(),
            GtEq => ">=".to_string(),
            Lt => "<".to_string(),
            LtEq => "<=".to_string(),
            Ident(str) | String(str) => str.to_string(),
            Number(num) => format!("{num}"),
            And => "and".to_string(),
            Class => "class".to_string(),
            Else => "else".to_string(),
            False => "false".to_string(),
            Fun => "fun".to_string(),
            For => "for".to_string(),
            If => "if".to_string(),
            Nil => "nil".to_string(),
            Or => "or".to_string(),
            Print => "print".to_string(),
            Return => "return".to_string(),
            Super => "super".to_string(),
            This => "this".to_string(),
            True => "true".to_string(),
            Var => "var".to_string(),
            While => "while".to_string(),
            Eof => "EOF".to_string(),
        }
    }
}
