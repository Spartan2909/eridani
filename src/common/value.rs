use crate::prelude::*;

#[derive(Debug, Clone)]
pub enum Value {
    Number(f64),
    String(String),
}

#[cfg(feature = "compiler")]
impl From<crate::compiler::scanner::Token> for Value {
    fn from(value: crate::compiler::scanner::Token) -> Self {
        use crate::compiler::{internal_error, scanner::TokenType};

        match value.kind() {
            TokenType::Number => Value::Number(
                value
                    .lexeme()
                    .parse::<f64>()
                    .unwrap_or_else(|_| internal_error!("parsed token {:?} as number", value)),
            ),
            TokenType::String => Value::String(value.lexeme().to_string()),
            _ => internal_error!("parsed token {:?} as literal", value),
        }
    }
}

#[derive(Debug, Clone, Copy)]
pub enum Type {
    Even,
    Integer,
    List,
    Number,
    Odd,
    String,
}

#[cfg(feature = "compiler")]
impl From<crate::compiler::scanner::Token> for Type {
    fn from(value: crate::compiler::scanner::Token) -> Self {
        match value.lexeme() {
            "Even" => Type::Even,
            "Integer" => Type::Integer,
            "List" => Type::List,
            "Number" => Type::Number,
            "Odd" => Type::Odd,
            "String" => Type::String,
            _ => crate::compiler::internal_error!("parsed '{:?}' as type", value),
        }
    }
}

#[derive(Debug, Clone, Copy)]
pub enum Comparision {
    Equal,
    NotEqual,
    Greater,
    GreaterEqual,
    Less,
    LessEqual,
}

#[derive(Debug, Clone, Copy)]
pub enum BinOp {
    And,
    Or,
}

#[derive(Debug, Clone, Copy)]
pub enum UnOp {
    Not,
}

#[derive(Debug, Clone)]
pub enum Pattern {
    Binary {
        left: Box<Pattern>,
        operator: BinOp,
        right: Box<Pattern>,
    },
    Comparision {
        comparison: Comparision,
        rhs: Value,
    },
    List {
        left: Box<Pattern>,
        right: Box<Pattern>,
    },
    Literal(Value),
    Range {
        lower: Value,
        upper: Value,
        inclusive: bool,
    },
    Type(Type),
    Unary {
        operator: UnOp,
        right: Box<Pattern>,
    },
    Wildcard(Option<String>),
}

#[cfg(feature = "compiler")]
impl From<crate::compiler::parser::Pattern> for Pattern {
    fn from(value: crate::compiler::parser::Pattern) -> Self {
        use crate::compiler::{
            internal_error, parser::Pattern as ParsePattern, scanner::TokenType,
        };

        match value {
            ParsePattern::Binary {
                left,
                operator,
                right,
            } => {
                let operator = match operator.kind() {
                    TokenType::Ampersand => BinOp::And,
                    TokenType::Pipe => BinOp::Or,
                    _ => internal_error!("parsed '{:?}' as pattern binary operator", operator),
                };

                Pattern::Binary {
                    left: Box::new((&*left).into()),
                    operator,
                    right: Box::new((&*right).into()),
                }
            }
            ParsePattern::Comparision { comparison, rhs } => {
                let comparison = match comparison.kind() {
                    TokenType::EqualEqual => Comparision::Equal,
                    TokenType::BangEqual => Comparision::NotEqual,
                    TokenType::Greater => Comparision::Greater,
                    TokenType::GreaterEqual => Comparision::GreaterEqual,
                    TokenType::Less => Comparision::Less,
                    TokenType::LessEqual => Comparision::LessEqual,
                    _ => internal_error!("parsed '{:?}' as comparison", comparison),
                };

                Pattern::Comparision {
                    comparison,
                    rhs: rhs.into(),
                }
            }
            ParsePattern::List { left, right } => Pattern::List {
                left: Box::new((&*left).into()),
                right: Box::new((&*right).into()),
            },
            ParsePattern::Literal(token) => Pattern::Literal(token.into()),
            ParsePattern::Range {
                lower,
                upper,
                inclusive,
            } => Pattern::Range {
                lower: lower.into(),
                upper: upper.into(),
                inclusive,
            },
            ParsePattern::Type(kind) => Pattern::Type(kind.into()),
            ParsePattern::Unary { operator, right } => {
                let operator = match operator.kind() {
                    TokenType::Bang => UnOp::Not,
                    _ => internal_error!("parsed '{:?}' as pattern unary operator", operator),
                };

                Pattern::Unary { operator, right: Box::new((&*right).into()) }
            },
            ParsePattern::Wildcard(token) => {
                match token.kind() {
                    TokenType::Identifier => Pattern::Wildcard(Some(token.lexeme().to_string())),
                    TokenType::Underscore => Pattern::Wildcard(None),
                    _ => internal_error!("parsed '{:?}' as wildcard", token),
                }
            }
        }
    }
}

#[cfg(feature = "compiler")]
impl From<&crate::compiler::parser::Pattern> for Pattern {
    fn from(value: &crate::compiler::parser::Pattern) -> Self {
        value.clone().into()
    }
}
