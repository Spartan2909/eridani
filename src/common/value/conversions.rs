use super::*;

use crate::compiler::{
    internal_error, parser,
    scanner::{Token, TokenType},
};

#[cfg(feature = "no_std")]
use crate::prelude::*;

impl From<Token> for Value {
    fn from(value: Token) -> Self {
        (&value).into()
    }
}

impl From<&Token> for Value {
    fn from(value: &Token) -> Self {
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

impl From<Token> for Type {
    fn from(value: Token) -> Self {
        match value.lexeme() {
            "Integer" => Type::Integer,
            "List" => Type::List,
            "Number" => Type::Number,
            "String" => Type::String,
            _ => internal_error!("parsed '{:?}' as type", value),
        }
    }
}

impl From<parser::Pattern> for Pattern {
    fn from(value: parser::Pattern) -> Self {
        match value {
            parser::Pattern::Binary {
                left,
                operator,
                right,
            } => {
                let operator = match operator.kind() {
                    TokenType::Ampersand => LogOp::And,
                    TokenType::Pipe => LogOp::Or,
                    _ => internal_error!("parsed '{:?}' as pattern binary operator", operator),
                };

                Pattern::Binary {
                    left: Box::new((&*left).into()),
                    operator,
                    right: Box::new((&*right).into()),
                }
            }
            parser::Pattern::Comparision { comparison, rhs } => {
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
            parser::Pattern::List { left, right } => Pattern::List {
                left: Box::new((&*left).into()),
                right: Box::new((&*right).into()),
            },
            parser::Pattern::Literal(token) => Pattern::Literal(token.into()),
            parser::Pattern::OperatorComparison {
                operator,
                mid,
                comparison,
                rhs,
            } => {
                let operator = match operator.kind() {
                    TokenType::Plus => ArithOp::Add,
                    TokenType::Minus => ArithOp::Sub,
                    TokenType::Star => ArithOp::Mul,
                    TokenType::Slash => ArithOp::Div,
                    TokenType::Mod => ArithOp::Mod,
                    _ => internal_error!("parsed token '{:?}' as operator", operator),
                };
                let mid = mid.into();
                let comparison = match comparison.kind() {
                    TokenType::EqualEqual => Comparision::Equal,
                    TokenType::BangEqual => Comparision::NotEqual,
                    TokenType::Greater => Comparision::Greater,
                    TokenType::GreaterEqual => Comparision::GreaterEqual,
                    TokenType::Less => Comparision::Less,
                    TokenType::LessEqual => Comparision::LessEqual,
                    _ => internal_error!("parsed '{:?}' as comparison", comparison),
                };
                let rhs = rhs.into();
                Pattern::OperatorComparison {
                    operator,
                    mid,
                    comparison,
                    rhs,
                }
            }
            parser::Pattern::Range {
                lower,
                upper,
                inclusive,
            } => Pattern::Range {
                lower: lower.into(),
                upper: upper.into(),
                inclusive,
            },
            parser::Pattern::Type(kind) => Pattern::Type(kind.into()),
            parser::Pattern::Unary { operator, right } => {
                let operator = match operator.kind() {
                    TokenType::Bang => UnOp::Not,
                    _ => internal_error!("parsed '{:?}' as pattern unary operator", operator),
                };

                Pattern::Unary {
                    operator,
                    right: Box::new((&*right).into()),
                }
            }
            parser::Pattern::Wildcard(token) => match token.kind() {
                TokenType::Identifier => Pattern::Wildcard(Some(token.lexeme().to_string())),
                TokenType::Underscore => Pattern::Wildcard(None),
                _ => internal_error!("parsed '{:?}' as wildcard", token),
            },
        }
    }
}

impl From<&parser::Pattern> for Pattern {
    fn from(value: &parser::Pattern) -> Self {
        value.clone().into()
    }
}
