use super::*;

use crate::compiler::scanner::{Token, TokenType};

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
            TokenType::String => Value::String(value.lexeme().to_string()),
            _ => internal_error!("parsed token {:?} as literal", value),
        }
    }
}

impl From<Token> for Value {
    fn from(value: Token) -> Self {
        (&value).into()
    }
}
