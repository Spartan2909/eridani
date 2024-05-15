use crate::{
    common::{bytecode::Method, internal_error},
    prelude::*,
};

use core::{
    cmp::Ordering,
    fmt,
    hash::{self, Hash},
    mem,
    ops::{Add, Div, Mul, Neg, Rem, Sub},
    ptr,
    str::FromStr,
};

use alloc::collections::VecDeque;

#[cfg(feature = "serialise")]
use serde::{Deserialize, Serialize};

use strum::EnumCount;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
#[cfg_attr(feature = "serialise", derive(Serialize, Deserialize))]
pub(crate) enum FunctionKind {
    Eridani,
    Native,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
#[cfg_attr(feature = "serialise", derive(Serialize, Deserialize))]
pub struct FunctionRef {
    pub(crate) reference: u16,
    pub(crate) kind: FunctionKind,
}

#[derive(Debug, Clone)]
#[cfg_attr(feature = "serialise", derive(Serialize, Deserialize))]
#[non_exhaustive]
pub enum Value {
    Function(FunctionRef),
    List(VecDeque<Value>),
    Nothing,
    #[doc(hidden)]
    Method(Box<Method>),
    Number(f64),
    String(String),
}

impl Hash for Value {
    fn hash<H: hash::Hasher>(&self, state: &mut H) {
        mem::discriminant(self).hash(state);
        match self {
            Value::Function(func_ref) => func_ref.hash(state),
            Value::List(list) => list.hash(state),
            Value::Nothing => {}
            Value::Method(method) => (method.as_ref() as *const Method).hash(state),
            Value::Number(n) => n.to_bits().hash(state),
            Value::String(s) => s.hash(state),
        }
    }
}

impl Add for &Value {
    type Output = Option<Value>;

    fn add(self, rhs: Self) -> Self::Output {
        match (self, rhs) {
            (Value::Number(n1), Value::Number(n2)) => Some(Value::Number(n1 + n2)),
            (Value::String(s1), Value::String(s2)) => Some(Value::String(s1.to_owned() + s2)),
            _ => None,
        }
    }
}

impl Sub for &Value {
    type Output = Option<Value>;

    fn sub(self, rhs: Self) -> Self::Output {
        match (self, rhs) {
            (Value::Number(n1), Value::Number(n2)) => Some(Value::Number(n1 - n2)),
            _ => None,
        }
    }
}

impl Mul for &Value {
    type Output = Option<Value>;

    fn mul(self, rhs: Self) -> Self::Output {
        match (self, rhs) {
            (Value::Number(n1), Value::Number(n2)) => Some(Value::Number(n1 * n2)),
            _ => None,
        }
    }
}

impl Div for &Value {
    type Output = Option<Value>;

    fn div(self, rhs: Self) -> Self::Output {
        match (self, rhs) {
            (Value::Number(n1), Value::Number(n2)) => Some(Value::Number(n1 / n2)),
            _ => None,
        }
    }
}

impl Rem for &Value {
    type Output = Option<Value>;

    fn rem(self, rhs: Self) -> Self::Output {
        match (self, rhs) {
            (Value::Number(n1), Value::Number(n2)) => Some(Value::Number(n1 % n2)),
            _ => None,
        }
    }
}

impl Neg for &Value {
    type Output = Option<Value>;

    fn neg(self) -> Self::Output {
        if let Value::Number(n) = self {
            Some(Value::Number(-n))
        } else {
            None
        }
    }
}

impl Neg for Value {
    type Output = Option<Value>;

    fn neg(self) -> Self::Output {
        -&self
    }
}

impl_binop_traits! {
    (Add, add, +),
    (Sub, sub, -),
    (Mul, mul, *),
    (Div, div, /),
    (Rem, rem, %),
}

macro_rules! impl_binop_traits {
    { $( ($trait_name:ident, $function_name:ident, $operator:tt), )+ } => {
        $(
            impl $trait_name for Value {
                type Output = Option<Value>;

                fn $function_name(self, other: Self) -> Self::Output {
                    &self $operator &other
                }
            }

            impl $trait_name<&Value> for Value {
                type Output = Option<Value>;

                fn $function_name(self, other: &Value) -> Self::Output {
                    &self $operator other
                }
            }

            impl $trait_name<Value> for &Value {
                type Output = Option<Value>;

                fn $function_name(self, other: Value) -> Self::Output {
                    self $operator &other
                }
            }
        )+
    };
}

use impl_binop_traits;

impl fmt::Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Value::Function(_) => write!(f, "<function>"),
            Value::List(l) => {
                write!(f, "[")?;
                for (i, value) in l.iter().enumerate() {
                    if value.is_string() {
                        write!(f, "'{value}'")?;
                    } else {
                        write!(f, "{value}")?;
                    }
                    if i < l.len() - 1 {
                        write!(f, ", ")?;
                    }
                }
                write!(f, "]")
            }
            Value::Method(_) => write!(f, "method"),
            Value::Nothing => write!(f, "nothing"),
            Value::Number(n) => write!(f, "{n}"),
            Value::String(s) => write!(f, "{s}"),
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
            (Value::Function(f1), Value::Function(f2)) => f1 == f2,
            (Value::Method(m1), Value::Method(m2)) => {
                let ptr1: *const _ = m1;
                let ptr2: *const _ = m2;
                ptr1 == ptr2
            }
            _ => false,
        }
    }
}

impl PartialEq<&Value> for Value {
    fn eq(&self, other: &&Value) -> bool {
        self == *other
    }
}

impl PartialEq<Value> for &Value {
    fn eq(&self, other: &Value) -> bool {
        *self == other
    }
}

impl Eq for Value {}

impl PartialOrd for Value {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        match (self, other) {
            (Value::List(l1), Value::List(l2)) => l1.partial_cmp(l2),
            (Value::Nothing, Value::Nothing) => Some(Ordering::Equal),
            (Value::Number(n1), Value::Number(n2)) => n1.partial_cmp(n2),
            (Value::String(s1), Value::String(s2)) => s1.partial_cmp(s2),
            (Value::Function(f1), Value::Function(f2)) if f1 == f2 => Some(Ordering::Equal),
            (Value::Method(m1), Value::Method(m2)) if ptr::eq(m1, m2) => Some(Ordering::Equal),
            _ => None,
        }
    }
}

impl FromStr for Value {
    type Err = ();

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        if let Ok(n) = s.parse() {
            Ok(Value::Number(n))
        } else if (s.starts_with('"') && s.ends_with('"')
            || s.starts_with('\'') && s.ends_with('\''))
            && s.len() > 1
        {
            Ok(Value::String(s[1..s.len() - 1].to_owned()))
        } else if s == "nothing" {
            Ok(Value::Nothing)
        } else if s.starts_with('[') && s.ends_with(']') {
            let s = &s[1..s.len() - 1];
            let mut values = VecDeque::with_capacity(s.matches(',').count() + 1);
            for value in s.split(',') {
                values.push_front(value.parse()?);
            }
            Ok(Value::List(values))
        } else {
            Err(())
        }
    }
}

impl Value {
    #[must_use]
    pub const fn as_number(&self) -> Option<f64> {
        if let Value::Number(n) = self {
            Some(*n)
        } else {
            None
        }
    }

    pub(crate) fn expect_number(&self) -> f64 {
        self.as_number()
            .unwrap_or_else(|| internal_error!("called 'expect_number' on {:?}", self))
    }

    #[must_use]
    pub const fn is_something(&self) -> bool {
        !matches!(self, Value::Nothing)
    }

    #[must_use]
    pub const fn as_string(&self) -> Option<&String> {
        if let Value::String(s) = self {
            Some(s)
        } else {
            None
        }
    }

    pub(crate) fn expect_string(&self) -> &String {
        self.as_string()
            .unwrap_or_else(|| internal_error!("called 'expect_string' on {:?}", self))
    }

    pub(crate) fn into_string(self) -> String {
        if let Value::String(s) = self {
            s
        } else {
            internal_error!("called 'expect_string' on {:?}", self)
        }
    }

    #[must_use]
    pub const fn is_string(&self) -> bool {
        matches!(self, Value::String(_))
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, EnumCount)]
#[repr(u8)]
pub(crate) enum Type {
    Callable,
    Integer,
    List,
    Number,
    String,
}

impl Type {
    pub(crate) fn is_kind(self, value: &Value) -> bool {
        match (self, value) {
            (Type::Integer, Value::Number(num)) => num.fract() == 0.0,
            (Type::List, Value::List(_))
            | (Type::Number, Value::Number(_))
            | (Type::String, Value::String(_))
            | (Type::Callable, Value::Function(_) | Value::Method(_)) => true,
            _ => false,
        }
    }
}

impl From<u8> for Type {
    fn from(value: u8) -> Self {
        if value as usize >= Type::COUNT {
            internal_error!("cannot interpret '0b{:08b}' as type", value);
        }
        // SAFETY: guaranteed by condition
        unsafe { mem::transmute(value) }
    }
}

impl From<Type> for u8 {
    fn from(value: Type) -> Self {
        value as u8
    }
}

#[cfg(feature = "compiler")]
mod conversions;
