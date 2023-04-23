use crate::prelude::*;

use alloc::collections::BTreeMap;
use core::{
    cmp::Ordering,
    fmt,
    ops::{Add, Div, Mul, Rem, Sub},
};

#[derive(Debug, Clone)]
pub enum Value {
    List(Vec<Value>),
    Nothing,
    Number(f64),
    String(String),
}

impl From<Option<Value>> for Value {
    fn from(value: Option<Value>) -> Self {
        match value {
            Some(value) => value,
            None => Value::Nothing,
        }
    }
}

impl From<Option<&Value>> for Value {
    fn from(value: Option<&Value>) -> Self {
        value.map(|value| value.clone()).into()
    }
}

impl Add for Value {
    type Output = Option<Value>;

    fn add(self, rhs: Self) -> Self::Output {
        match (self, rhs) {
            (Value::Number(n1), Value::Number(n2)) => Some(Value::Number(n1 + n2)),
            (Value::String(s1), Value::String(s2)) => Some(Value::String(s1 + &s2)),
            _ => None,
        }
    }
}

impl Sub for Value {
    type Output = Option<Value>;

    fn sub(self, rhs: Self) -> Self::Output {
        match (self, rhs) {
            (Value::Number(n1), Value::Number(n2)) => Some(Value::Number(n1 - n2)),
            _ => None,
        }
    }
}

impl Mul for Value {
    type Output = Option<Value>;

    fn mul(self, rhs: Self) -> Self::Output {
        match (self, rhs) {
            (Value::Number(n1), Value::Number(n2)) => Some(Value::Number(n1 * n2)),
            _ => None,
        }
    }
}

impl Div for Value {
    type Output = Option<Value>;

    fn div(self, rhs: Self) -> Self::Output {
        match (self, rhs) {
            (Value::Number(n1), Value::Number(n2)) => Some(Value::Number(n1 / n2)),
            _ => None,
        }
    }
}

impl Rem for Value {
    type Output = Option<Value>;

    fn rem(self, rhs: Self) -> Self::Output {
        match (self, rhs) {
            (Value::Number(n1), Value::Number(n2)) => Some(Value::Number(n1 % n2)),
            _ => None,
        }
    }
}

impl fmt::Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Value::List(l) => {
                write!(f, "[")?;
                for value in l {
                    write!(f, "{value}")?;
                    if Some(value) != l.last() {
                        write!(f, ", ")?;
                    }
                }
                write!(f, "]")
            }
            Value::Nothing => write!(f, "nothing"),
            Value::Number(n) => write!(f, "{n}"),
            Value::String(s) => write!(f, "\"{s}\""),
        }
    }
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

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Type {
    Integer,
    List,
    Number,
    String,
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
pub enum Pattern {
    Binary {
        left: Box<Pattern>,
        operator: LogOp,
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
    OperatorComparison {
        operator: ArithOp,
        mid: Value,
        comparison: Comparision,
        rhs: Value,
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

impl Pattern {
    pub fn matches(
        &self,
        value: &Value,
        bindings: &BTreeMap<String, Value>,
    ) -> Option<BTreeMap<String, Value>> {
        match self {
            Pattern::Binary {
                left,
                operator,
                right,
            } => {
                let mut new_bindings = left.matches(value, bindings)?;
                new_bindings.extend(bindings.clone());
                if *operator == LogOp::Or {
                    Some(new_bindings)
                } else {
                    new_bindings.extend(right.matches(value, bindings)?);
                    Some(new_bindings)
                }
            }
            Pattern::Comparision { comparison, rhs } => {
                let matches = comparison.compare(value, rhs);

                if matches {
                    Some(bindings.clone())
                } else {
                    None
                }
            }
            Pattern::List { left, right } => {
                if let Value::List(list) = value {
                    let mut new_bindings = left.matches(value, bindings)?;
                    let tail = match list.get(1) {
                        Some(_) => Value::List(list[1..].to_vec()),
                        None => Value::Nothing,
                    };
                    new_bindings.extend(right.matches(&tail, bindings)?);
                    Some(new_bindings)
                } else {
                    None
                }
            }
            Pattern::Literal(literal) => {
                if value == literal {
                    Some(bindings.clone())
                } else {
                    None
                }
            }
            Pattern::OperatorComparison {
                operator,
                mid,
                comparison,
                rhs,
            } => {
                let (value, mid) = (value.clone(), mid.clone());

                let value = match *operator {
                    ArithOp::Add => value + mid,
                    ArithOp::Sub => value - mid,
                    ArithOp::Mul => value * mid,
                    ArithOp::Div => value / mid,
                    ArithOp::Mod => value % mid,
                }?;

                let matches = comparison.compare(&value, rhs);

                if matches {
                    Some(bindings.clone())
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
                    Some(bindings.clone())
                } else {
                    None
                }
            }
            Pattern::Type(kind) => {
                if value.is_kind(*kind) {
                    Some(bindings.clone())
                } else {
                    None
                }
            }
            Pattern::Unary { operator, right } => match *operator {
                UnOp::Not => {
                    if right.matches(value, bindings).is_some() {
                        None
                    } else {
                        Some(bindings.clone())
                    }
                }
            },
            Pattern::Wildcard(label) => {
                let mut bindings = bindings.clone();
                if let Some(label) = label {
                    if let Some(existing_value) = bindings.get(label) {
                        if value != existing_value {
                            return None;
                        }
                    }

                    bindings.insert(label.clone(), value.clone());
                }

                Some(bindings)
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
            Pattern::Comparision { .. } => vec![],
            Pattern::List { left, right } => {
                let mut bindings = left.bindings();
                bindings.extend(right.bindings());
                bindings
            }
            Pattern::Literal(_) => vec![],
            Pattern::OperatorComparison { .. } => vec![],
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
}

#[cfg(feature = "compiler")]
mod conversions;
