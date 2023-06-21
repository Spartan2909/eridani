use core::fmt;

use super::{Error, Result};

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum TokenType {
    // Single-character tokens
    LeftParen,
    RightParen,
    LeftBracket,
    RightBracket,
    Comma,
    Minus,
    Plus,
    Slash,
    Star,
    Colon,
    Pipe,
    Ampersand,
    Underscore,
    Mod,
    Newline,

    // Variable length tokens
    Bang,
    BangEqual,
    Equal,
    EqualEqual,
    Greater,
    GreaterEqual,
    Less,
    LessEqual,
    DotDot,
    DotDotEqual,

    // Literals
    Identifier,
    Type,
    String,
    Number,

    // Keywords
    And,
    Do,
    End,
    In,
    Is,
    Lambda,
    Let,
    Module,
    Nothing,
    Public,
    Super,
    Use,

    Eof,
}

impl fmt::Display for TokenType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{self:?}")
    }
}

pub trait OptionalKind {
    fn optional_kind(&self) -> Option<TokenType>;
}

#[derive(Debug, Clone, PartialEq)]
pub struct Token {
    kind: TokenType,
    lexeme: String,
    line: usize,
}

impl Token {
    fn new(kind: TokenType, lexeme: &str, line: usize) -> Self {
        Token {
            kind,
            lexeme: lexeme.to_string(),
            line,
        }
    }

    pub fn kind(&self) -> TokenType {
        self.kind
    }

    pub fn lexeme(&self) -> &String {
        &self.lexeme
    }

    pub fn line(&self) -> usize {
        self.line
    }
}

fn is_alpha(c: char) -> bool {
    c.is_ascii_alphabetic() || c == '_'
}

impl OptionalKind for Option<Token> {
    fn optional_kind(&self) -> Option<TokenType> {
        Some(self.clone()?.kind)
    }
}

impl OptionalKind for Option<&Token> {
    fn optional_kind(&self) -> Option<TokenType> {
        Some((*self)?.kind)
    }
}

struct Scanner {
    source: String,
    start: usize,
    current: usize,
    line: usize,
}

impl Scanner {
    fn new(source: &str) -> Self {
        Scanner {
            source: source.to_string(),
            start: 0,
            current: 0,
            line: 1,
        }
    }

    fn make_token(&self, kind: TokenType) -> Token {
        Token::new(kind, &self.source[self.start..self.current], self.line)
    }

    fn is_at_end(&mut self) -> bool {
        self.current >= self.source.len() - 1
    }

    fn advance(&mut self) -> char {
        self.current += 1;
        self.source.chars().nth(self.current - 1).unwrap()
    }

    fn peek(&self) -> char {
        self.source.chars().nth(self.current).unwrap_or('\0')
    }

    fn peek_next(&self) -> char {
        self.source.chars().nth(self.current + 1).unwrap_or('\0')
    }

    fn match_char(&mut self, expected: char) -> bool {
        if self.peek() != expected {
            return false;
        }

        self.current += 1;
        true
    }

    fn nth_char(&self, n: usize) -> char {
        self.source.chars().nth(n).unwrap()
    }

    fn skip_whitespace(&mut self) {
        loop {
            let c = self.peek();
            match c {
                '\n' => {
                    return;
                }
                c if c.is_whitespace() => {
                    self.advance();
                }
                '/' if self.peek_next() == '/' => {
                    while self.peek() != '\n' && !self.is_at_end() {
                        self.advance();
                    }
                }
                _ => return,
            }
        }
    }

    fn check_keyword(&self, start: usize, rest: &str, kind: TokenType) -> TokenType {
        let first_char_location = self.start + start;
        let length = rest.len();
        if self.current - self.start == start + length
            && &self.source[first_char_location..first_char_location + length] == rest
        {
            kind
        } else {
            TokenType::Identifier
        }
    }

    fn identifier_type(&self) -> TokenType {
        match self.nth_char(self.start) {
            'a' => self.check_keyword(1, "nd", TokenType::And),
            'c' => self.check_keyword(1, "allable", TokenType::Type),
            'd' => self.check_keyword(1, "o", TokenType::Do),
            'e' => self.check_keyword(1, "nd", TokenType::End),
            'i' => match self.nth_char(self.start + 1) {
                'n' => self.check_keyword(2, "", TokenType::In),
                's' => self.check_keyword(2, "", TokenType::Is),
                _ => TokenType::Identifier,
            },
            'l' => match self.nth_char(self.start + 1) {
                'a' => self.check_keyword(2, "mbda", TokenType::Lambda),
                'e' => self.check_keyword(2, "t", TokenType::Let),
                _ => TokenType::Identifier,
            },
            'm' => self.check_keyword(1, "odule", TokenType::Module),
            'n' => self.check_keyword(1, "othing", TokenType::Nothing),
            'p' => self.check_keyword(1, "ublic", TokenType::Public),
            's' => self.check_keyword(1, "uper", TokenType::Super),
            'u' => self.check_keyword(1, "se", TokenType::Use),
            'I' => self.check_keyword(1, "nteger", TokenType::Type),
            'L' => self.check_keyword(1, "ist", TokenType::Type),
            'N' => self.check_keyword(1, "umber", TokenType::Type),
            'S' => self.check_keyword(1, "tring", TokenType::Type),
            '_' => self.check_keyword(1, "", TokenType::Underscore),
            _ => TokenType::Identifier,
        }
    }

    fn identifier(&mut self) -> Token {
        while self.peek().is_ascii_alphanumeric() || self.peek() == '_' {
            self.advance();
        }

        self.make_token(self.identifier_type())
    }

    fn string(&mut self) -> Result<Token> {
        while self.peek() != '"' && !self.is_at_end() {
            self.advance();
        }

        if self.is_at_end() {
            Err(Error::new(
                self.line,
                "Syntax",
                " at '\"'",
                "Unterminated string",
            ))
        } else {
            self.advance();
            Ok(self.make_token(TokenType::String))
        }
    }

    fn number(&mut self) -> Token {
        while self.peek().is_ascii_digit() {
            self.advance();
        }

        if self.peek() == '.' && self.peek_next().is_ascii_digit() {
            self.advance();

            while self.peek().is_ascii_digit() {
                self.advance();
            }
        }

        self.make_token(TokenType::Number)
    }

    fn scan_token(&mut self) -> Result<Token> {
        self.skip_whitespace();
        self.start = self.current;

        if self.is_at_end() {
            return Ok(self.make_token(TokenType::Eof));
        }

        let c = self.advance();
        if is_alpha(c) {
            return Ok(self.identifier());
        }
        if c.is_ascii_digit() {
            return Ok(self.number());
        }

        match c {
            '(' => Ok(self.make_token(TokenType::LeftParen)),
            ')' => Ok(self.make_token(TokenType::RightParen)),
            '[' => Ok(self.make_token(TokenType::LeftBracket)),
            ']' => Ok(self.make_token(TokenType::RightBracket)),
            ',' => Ok(self.make_token(TokenType::Comma)),
            '-' => Ok(self.make_token(TokenType::Minus)),
            '+' => Ok(self.make_token(TokenType::Plus)),
            '/' => Ok(self.make_token(TokenType::Slash)),
            '*' => Ok(self.make_token(TokenType::Star)),
            ':' => Ok(self.make_token(TokenType::Colon)),
            '|' => Ok(self.make_token(TokenType::Pipe)),
            '&' => Ok(self.make_token(TokenType::Ampersand)),
            '%' => Ok(self.make_token(TokenType::Mod)),
            '\n' => {
                self.line += 1;
                Ok(self.make_token(TokenType::Newline))
            }
            '=' => {
                let kind = if self.match_char('=') {
                    TokenType::EqualEqual
                } else {
                    TokenType::Equal
                };

                Ok(self.make_token(kind))
            }
            '!' => {
                let kind = if self.match_char('=') {
                    TokenType::BangEqual
                } else {
                    TokenType::Bang
                };

                Ok(self.make_token(kind))
            }
            '<' => {
                let kind = if self.match_char('=') {
                    TokenType::LessEqual
                } else {
                    TokenType::Less
                };

                Ok(self.make_token(kind))
            }
            '>' => {
                let kind = if self.match_char('=') {
                    TokenType::GreaterEqual
                } else {
                    TokenType::Greater
                };

                Ok(self.make_token(kind))
            }
            '.' if self.match_char('.') => {
                let kind = if self.match_char('=') {
                    TokenType::DotDotEqual
                } else {
                    TokenType::DotDot
                };

                Ok(self.make_token(kind))
            }
            '"' => self.string(),
            _ => {
                let location = format!(" at '{c}'");
                Err(Error::new(
                    self.line,
                    "Syntax",
                    &location,
                    "Unexpected character",
                ))
            }
        }
    }
}

pub fn scan(source: &str) -> Result<Vec<Token>> {
    let mut scanner = Scanner::new(source);
    let mut tokens = vec![];
    let mut err = vec![];

    loop {
        let token = match scanner.scan_token() {
            Ok(token) => token,
            Err(e) => {
                err.push(e);
                continue;
            }
        };
        let at_end = token.kind == TokenType::Eof;
        tokens.push(token);
        if at_end {
            break;
        }
    }

    if err.is_empty() {
        Ok(tokens)
    } else {
        Err(err.into())
    }
}
