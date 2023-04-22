use crate::{
    compiler::{
        scanner::{OptionalKind, Token, TokenType},
        Error, Result,
    },
    prelude::*,
};

#[derive(Debug, Clone)]
pub struct ParseTree {
    imports: Vec<ImportTree>,
    functions: Vec<Function>,
}

impl ParseTree {
    pub fn imports(&self) -> &Vec<ImportTree> {
        &self.imports
    }

    pub fn functions(&self) -> &Vec<Function> {
        &self.functions
    }
}

#[derive(Debug, Clone)]
pub struct Function {
    name: Token,
    methods: Vec<Method>,
}

impl Function {
    pub fn name(&self) -> &Token {
        &self.name
    }

    pub fn methods(&self) -> &Vec<Method> {
        &self.methods
    }
}

#[derive(Debug, Clone)]
pub struct Method {
    args: Vec<NamedPattern>,
    body: Expr,
}

impl Method {
    pub fn args(&self) -> &Vec<NamedPattern> {
        &self.args
    }

    pub fn body(&self) -> &Expr {
        &self.body
    }
}

#[derive(Debug, Clone)]
pub struct NamedPattern {
    name: Option<Token>,
    pattern: Pattern,
}

impl NamedPattern {
    pub fn name(&self) -> Option<String> {
        self.name.as_ref().map(|name| name.lexeme().to_string())
    }

    pub fn pattern(&self) -> &Pattern {
        &self.pattern
    }
}

#[derive(Debug, Clone)]
pub enum Pattern {
    Binary {
        left: Box<Pattern>,
        operator: Token,
        right: Box<Pattern>,
    },
    Comparision {
        comparison: Token,
        rhs: Token,
    },
    List {
        left: Box<Pattern>,
        right: Box<Pattern>,
    },
    Literal(Token),
    OperatorComparison {
        operator: Token,
        mid: Token,
        comparison: Token,
        rhs: Token,
    },
    Range {
        lower: Token,
        upper: Token,
        inclusive: bool,
    },
    Type(Token),
    Unary {
        operator: Token,
        right: Box<Pattern>,
    },
    Wildcard(Token),
}

#[derive(Debug, Clone)]
pub enum Expr {
    Binary {
        left: Box<Self>,
        operator: Token,
        right: Box<Self>,
    },
    Block {
        body: Vec<Self>,
        end: Token,
    },
    Call {
        callee: Box<Self>,
        paren: Token,
        arguments: Vec<Self>,
    },
    Grouping(Box<Self>),
    Let {
        pattern: Pattern,
        value: Box<Self>,
    },
    List {
        expressions: Vec<Self>,
        end: Token,
    },
    Literal(Token),
    Method(Box<Method>),
    Unary {
        operator: Token,
        right: Box<Self>,
    },
    Variable(ImportTree),
}

#[derive(Debug, Clone, PartialEq)]
pub struct ImportTree {
    name: Token,
    next: Option<Box<ImportTree>>,
}

impl ImportTree {
    pub fn line(&self) -> usize {
        if let Some(next) = &self.next {
            next.line()
        } else {
            self.name.line()
        }
    }

    pub fn name(&self) -> &Token {
        &self.name
    }

    pub fn next(&self) -> &Option<Box<ImportTree>> {
        &self.next
    }

    fn push(&mut self, name: Token) {
        if let Some(next) = &mut self.next {
            next.push(name);
        } else {
            self.next = Some(Box::new(Self { name, next: None }));
        }
    }
}

fn error(token: &Token, message: &'static str) -> Error {
    if token.kind() == TokenType::Eof {
        Error::new(token.line(), "Syntax", " at end", message)
    } else {
        Error::new(
            token.line(),
            "Syntax",
            &format!(" at '{}'", token.lexeme()),
            message,
        )
    }
}

macro_rules! match_token {
    ( $self:ident, $skip:expr, $( $kind:expr ),+ ) => {
        $( $self.match_token($kind, $skip) || )+ false
    };
}

macro_rules! check_next {
    ( $self:ident, $( $kind:expr ),+ ) => {
        $( $self.check_token($kind, 1) || )+ false
    };
}

struct Parser {
    tokens: Vec<Token>,
    current: usize,
}

impl Parser {
    fn new(tokens: Vec<Token>) -> Parser {
        Self { tokens, current: 0 }
    }

    fn peek(&self, offset: usize) -> Option<&Token> {
        self.tokens.get(self.current + offset)
    }

    fn previous(&self) -> &Token {
        &self.tokens[self.current - 1]
    }

    fn is_at_end(&self) -> bool {
        self.check_ignore_newlines(TokenType::Eof)
    }

    fn advance(&mut self) -> &Token {
        if !self.is_at_end() {
            self.current += 1;
        }

        self.previous()
    }

    fn check_token(&self, kind: TokenType, offset: usize) -> bool {
        self.peek(offset).optional_kind() == Some(kind)
    }

    fn check_ignore_newlines(&self, kind: TokenType) -> bool {
        let mut offset = 0;
        while self.check_token(TokenType::Newline, offset) {
            offset += 1;
        }
        self.check_token(kind, offset)
    }

    fn match_token(&mut self, kind: TokenType, skip_newlines: bool) -> bool {
        let mut offset = 0;
        if skip_newlines {
            while self.check_token(TokenType::Newline, offset) {
                offset += 1;
            }
        }

        if self.check_token(kind, offset) {
            for _ in 0..=offset {
                self.advance();
            }

            true
        } else {
            false
        }
    }

    fn consume(&mut self, kind: TokenType, message: &'static str) -> Result<Token> {
        if self.check_token(kind, 0) {
            Ok(self.advance().clone())
        } else {
            Err(error(self.peek(0).unwrap(), message))
        }
    }

    fn consume_any(&mut self, kinds: Vec<TokenType>, message: &'static str) -> Result<Token> {
        for kind in kinds {
            if self.check_token(kind, 0) {
                return Ok(self.advance().clone());
            }
        }

        Err(error(self.peek(0).unwrap(), message))
    }

    fn skip_newlines(&mut self) {
        while self.check_token(TokenType::Newline, 0) {
            self.advance();
        }
    }

    fn synchronise(&mut self) {
        self.advance();
        while !self.is_at_end() {
            if self.peek(1).optional_kind() == Some(TokenType::Is)
                || self.peek(0).optional_kind() == Some(TokenType::Use)
            {
                return;
            }

            self.advance();
        }
    }

    fn parse(&mut self) -> Result<ParseTree> {
        let mut imports = vec![];
        let mut functions = vec![];
        let mut errors = vec![];

        while !self.is_at_end() {
            let result: Result<()> = try {
                if self.match_token(TokenType::Use, true) {
                    imports.push(self.import("Expect function or module name after 'use'")?);
                } else {
                    functions.push(self.function()?);
                }
            };

            if let Err(err) = result {
                errors.push(err);
                self.synchronise();
            } else {
                self.skip_newlines();
            }
        }

        if errors.is_empty() {
            Ok(ParseTree { imports, functions })
        } else if errors.len() == 1 {
            Err(errors[0].clone())
        } else {
            Err(Error::Collection(errors))
        }
    }

    fn import(&mut self, message: &'static str) -> Result<ImportTree> {
        let name = self.consume(TokenType::Identifier, message)?;

        let mut imports = vec![name];
        while self.match_token(TokenType::In, true) {
            imports.push(self.consume(TokenType::Identifier, "Expect module name after 'in'")?);
        }
        imports.reverse();
        let mut tree = ImportTree {
            name: imports[0].clone(),
            next: None,
        };
        for import in &imports[1..] {
            tree.push(import.clone())
        }

        Ok(tree)
    }

    fn function(&mut self) -> Result<Function> {
        let name = self.consume(TokenType::Identifier, "Expect function name")?;
        self.consume(TokenType::Is, "Expect 'is' after function name")?;

        let mut methods = vec![];

        while self.match_token(TokenType::LeftParen, true) {
            methods.push(self.method()?);
            self.consume_any(
                vec![TokenType::Newline, TokenType::Eof],
                "Expect newline after method",
            )?;
        }

        Ok(Function { name, methods })
    }

    fn method(&mut self) -> Result<Method> {
        let mut args = vec![];

        if !self.check_token(TokenType::RightParen, 0) {
            loop {
                let name = if self.peek(1).optional_kind() == Some(TokenType::Colon) {
                    let name = self.consume(TokenType::Identifier, "Expect pattern name")?;
                    self.consume(TokenType::Colon, "Cannot fail: already checked for colon")?;
                    Some(name)
                } else {
                    None
                };
                args.push(NamedPattern {
                    name,
                    pattern: self.pattern()?,
                });

                if !self.match_token(TokenType::Comma, true) {
                    break;
                }
            }
        }
        self.consume(TokenType::RightParen, "Expect ')' after method parameters")?;

        let body = self.expression()?;

        Ok(Method { args, body })
    }

    fn pattern(&mut self) -> Result<Pattern> {
        self.or()
    }

    fn or(&mut self) -> Result<Pattern> {
        let mut pattern = self.and()?;

        while self.match_token(TokenType::Pipe, true) {
            let operator = self.previous().clone();
            let right = Box::new(self.and()?);
            pattern = Pattern::Binary {
                left: Box::new(pattern),
                operator,
                right,
            }
        }

        Ok(pattern)
    }

    fn and(&mut self) -> Result<Pattern> {
        let mut pattern = self.operator_comparison()?;

        while self.match_token(TokenType::Ampersand, true) {
            let operator = self.previous().clone();
            let right = Box::new(self.comparison()?);
            pattern = Pattern::Binary {
                left: Box::new(pattern),
                operator,
                right,
            }
        }

        Ok(pattern)
    }

    fn operator_comparison(&mut self) -> Result<Pattern> {
        if match_token!(
            self,
            true,
            TokenType::Plus,
            TokenType::Minus,
            TokenType::Star,
            TokenType::Slash,
            TokenType::Mod
        ) {
            let operator = self.previous().clone();
            let mid = self.consume_any(
                vec![TokenType::Number, TokenType::String],
                "Expect literal after operator",
            )?;
            let comparison = self.consume_any(
                vec![
                    TokenType::BangEqual,
                    TokenType::EqualEqual,
                    TokenType::Greater,
                    TokenType::GreaterEqual,
                    TokenType::Less,
                    TokenType::LessEqual,
                ],
                "Expect comparison",
            )?;
            let rhs = self.consume_any(
                vec![TokenType::Number, TokenType::String],
                "Expect literal after comparison",
            )?;

            Ok(Pattern::OperatorComparison {
                operator,
                mid,
                comparison,
                rhs,
            })
        } else {
            self.comparison()
        }
    }

    fn comparison(&mut self) -> Result<Pattern> {
        if match_token!(
            self,
            true,
            TokenType::BangEqual,
            TokenType::EqualEqual,
            TokenType::Greater,
            TokenType::GreaterEqual,
            TokenType::Less,
            TokenType::LessEqual
        ) {
            let comparison = self.previous().clone();
            let rhs = self.consume_any(
                vec![TokenType::Number, TokenType::String],
                "Expect literal after comparison",
            )?;

            Ok(Pattern::Comparision { comparison, rhs })
        } else {
            self.range()
        }
    }

    fn range(&mut self) -> Result<Pattern> {
        if check_next!(self, TokenType::DotDot, TokenType::DotDotEqual)
            && self.match_token(TokenType::Number, true)
        {
            let lower = self.previous().clone();
            let inclusive = !self.match_token(TokenType::DotDot, false);
            let upper = self.consume(TokenType::Number, "Expect number after range operator")?;

            Ok(Pattern::Range {
                lower,
                upper,
                inclusive,
            })
        } else {
            self.pattern_grouping()
        }
    }

    fn pattern_grouping(&mut self) -> Result<Pattern> {
        if self.match_token(TokenType::LeftParen, true) {
            let pattern = self.pattern()?;
            self.consume(TokenType::RightParen, "Expect ')' after grouping")?;
            Ok(pattern)
        } else {
            self.pattern_list()
        }
    }

    fn pattern_list(&mut self) -> Result<Pattern> {
        if self.match_token(TokenType::LeftBracket, true) {
            let left = Box::new(self.pattern()?);
            self.consume(
                TokenType::Comma,
                "Expect ',' after first element of list pattern",
            )?;
            let right = Box::new(self.pattern()?);
            self.consume(TokenType::RightBracket, "Expect ']' after list pattern")?;

            Ok(Pattern::List { left, right })
        } else {
            self.pattern_unary()
        }
    }

    fn pattern_unary(&mut self) -> Result<Pattern> {
        if self.match_token(TokenType::Bang, true) {
            let operator = self.previous().clone();
            let right = Box::new(self.pattern()?);
            Ok(Pattern::Unary { operator, right })
        } else {
            self.pattern_primary()
        }
    }

    fn pattern_primary(&mut self) -> Result<Pattern> {
        if match_token!(self, true, TokenType::Number, TokenType::String) {
            Ok(Pattern::Literal(self.previous().clone()))
        } else if self.match_token(TokenType::Identifier, true)
            || self.match_token(TokenType::Underscore, true)
        {
            let token = self.previous().clone();
            Ok(Pattern::Wildcard(token))
        } else if self.match_token(TokenType::Type, true) {
            Ok(Pattern::Type(self.previous().clone()))
        } else {
            Err(error(
                self.peek(0).unwrap(),
                "Expect literal, identifier, or '_'",
            ))
        }
    }

    fn expression(&mut self) -> Result<Expr> {
        self.assign()
    }

    fn assign(&mut self) -> Result<Expr> {
        if self.match_token(TokenType::Let, true) {
            let pattern = self.pattern()?;
            self.consume(
                TokenType::Equal,
                "Expect '=' after pattern in let expression",
            )?;
            let value = Box::new(self.expression()?);
            Ok(Expr::Let { pattern, value })
        } else {
            self.term()
        }
    }

    fn term(&mut self) -> Result<Expr> {
        let mut expr = self.factor()?;

        while match_token!(self, true, TokenType::Plus, TokenType::Minus) {
            let operator = self.previous().clone();
            let right = Box::new(self.factor()?);
            expr = Expr::Binary {
                left: Box::new(expr),
                operator,
                right,
            };
        }

        Ok(expr)
    }

    fn factor(&mut self) -> Result<Expr> {
        let mut expr = self.unary()?;

        while match_token!(
            self,
            true,
            TokenType::Star,
            TokenType::Slash,
            TokenType::Mod
        ) {
            let operator = self.previous().clone();
            let right = Box::new(self.unary()?);
            expr = Expr::Binary {
                left: Box::new(expr),
                operator,
                right,
            };
        }

        Ok(expr)
    }

    fn unary(&mut self) -> Result<Expr> {
        if self.match_token(TokenType::Minus, true) {
            let operator = self.previous().clone();
            let right = Box::new(self.unary()?);
            Ok(Expr::Unary { operator, right })
        } else {
            self.list()
        }
    }

    fn list(&mut self) -> Result<Expr> {
        if self.match_token(TokenType::LeftBracket, true) {
            let mut expressions = vec![];

            if !self.check_token(TokenType::RightBracket, 0) {
                loop {
                    expressions.push(self.expression()?);

                    if !self.match_token(TokenType::Comma, true) {
                        break;
                    }
                }
            }

            let end = self.consume(TokenType::RightBracket, "Expect ']' after list")?;

            Ok(Expr::List { expressions, end })
        } else {
            self.method_expr()
        }
    }

    fn method_expr(&mut self) -> Result<Expr> {
        if self.match_token(TokenType::Method, true) {
            let method = Box::new(self.method()?);
            Ok(Expr::Method(method))
        } else {
            self.call()
        }
    }

    fn call(&mut self) -> Result<Expr> {
        let mut expr = self.primary()?;

        while self.match_token(TokenType::LeftParen, false) {
            expr = self.finish_call(expr)?;
        }

        Ok(expr)
    }

    fn finish_call(&mut self, callee: Expr) -> Result<Expr> {
        let mut arguments = vec![];

        if !self.check_token(TokenType::RightParen, 0) {
            loop {
                if arguments.len() >= 255 {
                    todo!("error: too many arguments");
                }

                arguments.push(self.expression()?);

                if !self.match_token(TokenType::Comma, true) {
                    break;
                }
            }
        }

        let paren = self.consume(TokenType::RightParen, "Expect ')' after arguments")?;

        Ok(Expr::Call {
            callee: Box::new(callee),
            paren,
            arguments,
        })
    }

    fn primary(&mut self) -> Result<Expr> {
        if match_token!(
            self,
            true,
            TokenType::Nothing,
            TokenType::Number,
            TokenType::String
        ) {
            Ok(Expr::Literal(self.previous().clone()))
        } else if self.match_token(TokenType::LeftParen, true) {
            let expr = self.expression()?;
            self.consume(TokenType::RightBracket, "Expect closing parenthesis")?;
            Ok(Expr::Grouping(Box::new(expr)))
        } else if self.match_token(TokenType::Do, true) {
            let mut body = vec![];
            while !self.check_ignore_newlines(TokenType::End) && !self.is_at_end() {
                self.skip_newlines();
                body.push(self.expression()?);
                self.skip_newlines();
            }
            let end = self.consume(TokenType::End, "Expect 'end' after block")?;
            Ok(Expr::Block { body, end })
        } else if self.check_token(TokenType::Identifier, 0) {
            let tree = self.import("Expect identifier")?;
            Ok(Expr::Variable(tree))
        } else {
            Err(error(self.peek(0).unwrap(), "Unexpected token"))
        }
    }
}

pub fn parse(tokens: Vec<Token>) -> Result<ParseTree> {
    Parser::new(tokens).parse()
}
