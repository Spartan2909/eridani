use crate::{
    common::{internal_error, value::Value},
    compiler::{
        parser,
        scanner::{Token, TokenType},
    },
    prelude::*,
};

use alloc::collections::BTreeMap;
use core::mem;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Type {
    Integer,
    List,
    Number,
    String,
}

fn is_kind(this: &Value, kind: Type) -> bool {
    match (this, kind) {
        (Value::Number(num), Type::Integer) => num.fract() == 0.0,
        (Value::List(_), Type::List) => true,
        (Value::Number(_), Type::Number) => true,
        (Value::String(_), Type::String) => true,
        _ => false,
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Comparision {
    Equal,
    NotEqual,
    Greater,
    GreaterEqual,
    Less,
    LessEqual,
}

impl Comparision {
    fn compare(self, lhs: &Value, rhs: &Value) -> bool {
        match self {
            Comparision::Equal => lhs == rhs,
            Comparision::Greater => lhs > rhs,
            Comparision::GreaterEqual => lhs >= rhs,
            Comparision::Less => lhs < rhs,
            Comparision::LessEqual => lhs <= rhs,
            Comparision::NotEqual => lhs != rhs,
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum LogOp {
    And,
    Or,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ArithOp {
    Add,
    Sub,
    Mul,
    Div,
    Mod,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum UnOp {
    Not,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Item {
    Value(Value),
    Wildcard(String),
}

impl Item {
    fn resolve<'a>(&'a self, bindings: &'a BTreeMap<String, Value>) -> Option<&'a Value> {
        match self {
            Item::Value(value) => Some(value),
            Item::Wildcard(name) => bindings.get(name),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct OperatorChain {
    operator: ArithOp,
    mid: Item,
    next: Option<Box<OperatorChain>>,
}

impl OperatorChain {
    fn bindings(&self, mut bindings: Vec<String>) -> Vec<String> {
        if let Item::Wildcard(name) = &self.mid {
            bindings.push(name.to_owned());
        }

        if let Some(next) = &self.next {
            next.bindings(bindings)
        } else {
            bindings
        }
    }
}

impl From<&parser::OperatorChain> for OperatorChain {
    fn from(value: &parser::OperatorChain) -> Self {
        let operator = match value.operator().kind() {
            TokenType::Plus => ArithOp::Add,
            TokenType::Minus => ArithOp::Sub,
            TokenType::Star => ArithOp::Mul,
            TokenType::Slash => ArithOp::Div,
            TokenType::Mod => ArithOp::Mod,
            _ => internal_error!("parsed token '{:?}' as operator", value.operator()),
        };

        let mid: Item = value.mid().into();

        let next = if let Some(next) = value.next() {
            Some(Box::new(next.into()))
        } else {
            None
        };

        OperatorChain { operator, mid, next }
    }
}

impl From<parser::OperatorChain> for OperatorChain {
    fn from(value: parser::OperatorChain) -> Self {
        (&value).into()
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Pattern {
    Binary {
        left: Box<Pattern>,
        operator: LogOp,
        right: Box<Pattern>,
    },
    Comparision {
        comparison: Comparision,
        rhs: Item,
    },
    List {
        left: Box<Pattern>,
        right: Box<Pattern>,
    },
    Literal(Value),
    OperatorComparison {
        operator_chain: OperatorChain,
        comparison: Comparision,
        rhs: Item,
    },
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

#[repr(u8)]
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
enum PatternPrecedence {
    Wildcard = 10,
    Type = 20,
    Comparision = 30,
    Range = 40,
    Literal = 50,
}

impl From<PatternPrecedence> for i32 {
    fn from(value: PatternPrecedence) -> Self {
        unsafe { mem::transmute::<_, u8>(value) }.into()
    }
}

impl Pattern {
    pub fn matches(&self, value: &Value, bindings: &mut BTreeMap<String, Value>) -> Option<()> {
        match self {
            Pattern::Binary {
                left,
                operator,
                right,
            } => {
                if *operator == LogOp::Or {
                    let left = left.matches(value, bindings);
                    let right = right.matches(value, bindings);
                    if left.is_some() || right.is_some() {
                        Some(())
                    } else {
                        None
                    }
                } else {
                    left.matches(value, bindings)?;
                    right.matches(value, bindings)?;
                    Some(())
                }
            }
            Pattern::Comparision { comparison, rhs } => {
                let rhs = rhs.resolve(bindings)?;
                let matches = comparison.compare(value, rhs);

                if matches {
                    Some(())
                } else {
                    None
                }
            }
            Pattern::List { left, right } => {
                if let Value::List(list) = value {
                    left.matches(value, bindings)?;
                    let tail = match list.get(1) {
                        Some(_) => Value::List(list[1..].to_vec()),
                        None => Value::Nothing,
                    };
                    right.matches(&tail, bindings)?;
                    Some(())
                } else {
                    None
                }
            }
            Pattern::Literal(literal) => {
                if value == literal {
                    Some(())
                } else {
                    None
                }
            }
            Pattern::OperatorComparison {
                operator_chain,
                comparison,
                rhs,
            } => {
                let mut operator_chain = Some(operator_chain);
                let mut value = value.to_owned();

                while let Some(operator) = operator_chain {
                    let mid = operator.mid.resolve(bindings)?.clone();

                    value = match operator.operator {
                        ArithOp::Add => value + mid,
                        ArithOp::Sub => value - mid,
                        ArithOp::Mul => value * mid,
                        ArithOp::Div => value / mid,
                        ArithOp::Mod => value % mid,
                    }?;

                    operator_chain = match &operator.next {
                        Some(next) => Some(&*next),
                        None => None,
                    }
                }

                let rhs = rhs.resolve(bindings)?;
                let matches = comparison.compare(&value, rhs);

                if matches {
                    Some(())
                } else {
                    None
                }
            }
            Pattern::Range {
                lower,
                upper,
                inclusive,
            } => {
                if lower > value {
                    return None;
                }

                let matches = if *inclusive {
                    value <= upper
                } else {
                    value < upper
                };

                if matches {
                    Some(())
                } else {
                    None
                }
            }
            Pattern::Type(kind) => {
                if is_kind(value, *kind) {
                    Some(())
                } else {
                    None
                }
            }
            Pattern::Unary { operator, right } => match *operator {
                UnOp::Not => {
                    if right.matches(value, bindings).is_some() {
                        None
                    } else {
                        Some(())
                    }
                }
            },
            Pattern::Wildcard(label) => {
                if let Some(label) = label {
                    if let Some(existing_value) = bindings.get(label) {
                        if value != existing_value {
                            return None;
                        }
                    } else {
                        bindings.insert(label.clone(), value.clone());
                    }
                }

                Some(())
            }
        }
    }

    pub fn bindings(&self) -> Vec<String> {
        match self {
            Pattern::Binary { left, right, .. } => {
                let mut bindings = left.bindings();
                bindings.extend(right.bindings());
                bindings
            }
            Pattern::Comparision { rhs, .. } => {
                if let Item::Wildcard(name) = rhs {
                    vec![name.to_owned()]
                } else {
                    vec![]
                }
            }
            Pattern::List { left, right } => {
                let mut bindings = left.bindings();
                bindings.extend(right.bindings());
                bindings
            }
            Pattern::Literal(_) => vec![],
            Pattern::OperatorComparison { operator_chain, rhs, .. } => {
                let mut bindings = operator_chain.bindings(vec![]);

                if let Item::Wildcard(name) = rhs {
                    bindings.push(name.to_owned())
                }

                bindings
            }
            Pattern::Range { .. } => vec![],
            Pattern::Type(_) => vec![],
            Pattern::Unary { right, .. } => right.bindings(),
            Pattern::Wildcard(binding) => {
                if let Some(binding) = binding {
                    vec![binding.clone()]
                } else {
                    vec![]
                }
            }
        }
    }

    pub fn precedence(&self, bindings: &[Option<String>]) -> i32 {
        match self {
            Pattern::Binary {
                left,
                operator,
                right,
            } => {
                let left = left.precedence(bindings);
                let right = right.precedence(bindings);
                if *operator == LogOp::And {
                    let highest = if left > right { left } else { right };

                    highest + 1
                } else {
                    let lowest = if left < right { left } else { right };

                    lowest - 1
                }
            }
            Pattern::Comparision { .. } => PatternPrecedence::Comparision.into(),
            Pattern::List { left, right } => {
                (left.precedence(bindings) + right.precedence(bindings)) / 2
            }
            Pattern::Literal(_) => PatternPrecedence::Literal.into(),
            Pattern::OperatorComparison { .. } => PatternPrecedence::Comparision.into(),
            Pattern::Range { .. } => PatternPrecedence::Range.into(),
            Pattern::Type(_) => PatternPrecedence::Type.into(),
            Pattern::Unary { right, .. } => {
                mem::variant_count::<PatternPrecedence>() as i32 * 10 - right.precedence(bindings)
            }
            Pattern::Wildcard(name) => {
                if bindings.contains(name) {
                    PatternPrecedence::Literal.into()
                } else {
                    PatternPrecedence::Wildcard.into()
                }
            }
        }
    }

    pub fn is_binary(&self) -> bool {
        matches!(self, Pattern::Binary { .. })
    }

    pub fn is_comparison(&self) -> bool {
        matches!(self, Pattern::Comparision { .. })
    }

    pub fn is_list(&self) -> bool {
        matches!(self, Pattern::List { .. })
    }

    pub fn is_literal(&self, bindings: &[Option<String>]) -> bool {
        if let Pattern::Wildcard(name) = self {
            bindings.contains(name)
        } else {
            matches!(self, Pattern::Literal(_))
        }
    }

    pub fn becomes_literal(&self) -> bool {
        if let Pattern::Binary {
            left,
            operator,
            right,
        } = self
        {
            left.is_literal(&[]) && right.is_literal(&[]) && *operator == LogOp::And
        } else {
            false
        }
    }

    pub fn is_operator_comparison(&self) -> bool {
        matches!(self, Pattern::OperatorComparison { .. })
    }

    pub fn is_type(&self) -> bool {
        matches!(self, Pattern::Type(_))
    }

    pub fn is_wildcard(&self, bindings: &[Option<String>]) -> bool {
        if let Pattern::Wildcard(name) = self {
            !bindings.contains(name)
        } else {
            false
        }
    }

    pub fn becomes_wildcard(&self) -> bool {
        if let Pattern::Binary {
            left,
            operator,
            right,
        } = self
        {
            left.is_wildcard(&[]) && right.is_wildcard(&[]) && *operator == LogOp::Or
        } else {
            false
        }
    }
}

impl From<&Token> for Item {
    fn from(value: &Token) -> Self {
        match value.kind() {
            TokenType::Identifier => Item::Wildcard(value.lexeme().to_owned()),
            TokenType::Nothing | TokenType::Number | TokenType::String => Item::Value(value.into()),
            _ => internal_error!("parsed token {:?} as literal or identifier", value),
        }
    }
}

impl From<Token> for Item {
    fn from(value: Token) -> Self {
        (&value).into()
    }
}

impl From<Token> for Type {
    fn from(value: Token) -> Self {
        match value.lexeme().as_str() {
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
                operator_chain,
                comparison,
                rhs,
            } => {
                let operator_chain = operator_chain.into();
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
                    operator_chain,
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
