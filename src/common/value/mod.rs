use crate::prelude::*;

use core::cmp::Ordering;

#[derive(Debug, Clone)]
pub enum Value {
    List(Box<List>),
    Nothing,
    Number(f64),
    String(String),
}

impl PartialEq for Value {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Value::List(l1), Value::List(l2)) => l1 == l2,
            (Value::Nothing, Value::Nothing) => true,
            (Value::Number(n1), Value::Number(n2)) => n1 == n2,
            (Value::String(s1), Value::String(s2)) => s1 == s2,
            _ => false,
        }
    }
}

impl PartialOrd for Value {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        match (self, other) {
            (Value::List(l1), Value::List(l2)) => l1.partial_cmp(l2),
            (Value::Nothing, Value::Nothing) => Some(Ordering::Equal),
            (Value::Number(n1), Value::Number(n2)) => n1.partial_cmp(n2),
            (Value::String(s1), Value::String(s2)) => s1.partial_cmp(s2),
            _ => None,
        }
    }
}

impl Value {
    fn is_kind(&self, kind: Type) -> bool {
        match (self, kind) {
            (Value::Number(num), Type::Integer) => num.fract() == 0.0,
            (Value::List(_), Type::List) => true,
            (Value::Number(_), Type::Number) => true,
            (Value::String(_), Type::String) => true,
            _ => false,
        }
    }
}

#[derive(Debug, Clone)]
pub struct List {
    head: Value,
    next: Option<Box<List>>,
}

impl PartialEq for List {
    fn eq(&self, other: &Self) -> bool {
        if self.head != other.head {
            return false;
        }

        match (&self.next, &other.next) {
            (Some(l1), Some(l2)) => l1 == l2,
            (None, None) => true,
            _ => false,
        }
    }
}

impl PartialOrd for List {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        if self == other {
            Some(Ordering::Equal)
        } else {
            None
        }
    }
}

#[derive(Debug, Clone, Copy)]
pub enum Type {
    Integer,
    List,
    Number,
    String,
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

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
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

impl Pattern {
    pub fn matches(
        &self,
        value: &Value,
        bindings: &[(String, Value)],
    ) -> Option<Vec<(String, Value)>> {
        match self {
            Pattern::Binary {
                left,
                operator,
                right,
            } => {
                let mut new_bindings = left.matches(value, bindings)?;
                new_bindings.extend(bindings.iter().cloned());
                if *operator == BinOp::Or {
                    Some(new_bindings)
                } else {
                    new_bindings.extend(right.matches(value, bindings)?.iter().cloned());
                    Some(new_bindings)
                }
            }
            Pattern::Comparision { comparison, rhs } => {
                let matches = match comparison {
                    Comparision::Equal => value == rhs,
                    Comparision::Greater => value > rhs,
                    Comparision::GreaterEqual => value >= rhs,
                    Comparision::Less => value < rhs,
                    Comparision::LessEqual => value <= rhs,
                    Comparision::NotEqual => value != rhs,
                };

                if matches {
                    Some(bindings.to_vec())
                } else {
                    None
                }
            }
            Pattern::List { left, right } => {
                if let Value::List(list) = value {
                    let mut new_bindings = left.matches(value, bindings)?;
                    let tail = match &list.next {
                        Some(next) => Value::List((*next).clone()),
                        None => Value::Nothing,
                    };
                    new_bindings.extend(right.matches(&tail, bindings)?.iter().cloned());
                    Some(new_bindings)
                } else {
                    None
                }
            }
            Pattern::Literal(literal) => {
                if value == literal {
                    Some(bindings.to_vec())
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
                    Some(bindings.to_vec())
                } else {
                    None
                }
            }
            Pattern::Type(kind) => {
                if value.is_kind(*kind) {
                    Some(bindings.to_vec())
                } else {
                    None
                }
            }
            Pattern::Unary { operator, right } => match *operator {
                UnOp::Not => {
                    if right.matches(value, bindings).is_some() {
                        None
                    } else {
                        Some(bindings.to_vec())
                    }
                }
            },
            Pattern::Wildcard(label) => {
                let mut bindings = bindings.to_vec();
                if let Some(label) = label {
                    bindings.push((label.clone(), value.clone()));
                }
                Some(bindings)
            }
        }
    }
}

mod conversions;
