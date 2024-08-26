use super::{Type, Value};

use crate::{
    common::internal_error,
    compiler::scanner::{Token, TokenType},
};

use alloc::string::ToString;

impl From<&Token> for Value {
    fn from(value: &Token) -> Self {
        match value.kind() {
            TokenType::Nothing => Value::Nothing,
            TokenType::Number => Value::Number(
                value
                    .lexeme()
                    .parse::<f64>()
                    .unwrap_or_else(|_| internal_error!("parsed token {:?} as number", value)),
            ),
            TokenType::String => {
                let mut string = value.lexeme()[1..].to_string();
                string.pop();
                Value::String(string)
            }
            _ => internal_error!("parsed token {:?} as literal", value),
        }
    }
}

impl From<Token> for Value {
    fn from(value: Token) -> Self {
        (&value).into()
    }
}

impl From<&Token> for Type {
    fn from(value: &Token) -> Self {
        match value.lexeme() {
            "Callable" => Type::Callable,
            "Integer" => Type::Integer,
            "List" => Type::List,
            "Number" => Type::Number,
            "String" => Type::String,
            _ => internal_error!("parsed '{:?}' as type", value),
        }
    }
}

impl From<Token> for Type {
    fn from(value: Token) -> Self {
        (&value).into()
    }
}
