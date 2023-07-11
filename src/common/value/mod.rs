use crate::common::internal_error;

#[cfg(feature = "tree_walk")]
use alloc::rc::Rc;

use core::{
    cmp::Ordering,
    fmt, mem,
    ops::{Add, Div, Mul, Neg, Rem, Sub},
    str::FromStr,
};

#[cfg(feature = "tree_walk")]
use core::cell::RefCell;

use strum::EnumCount;

#[derive(Debug, Clone)]
#[non_exhaustive]
pub enum Value {
    #[doc(hidden)]
    Function(Function),
    List(Vec<Value>),
    Nothing,
    #[doc(hidden)]
    Method(Method),
    Number(f64),
    String(String),
}

#[cfg(feature = "tree_walk")]
type Function = Rc<RefCell<crate::compiler::analyser::Function>>;

#[cfg(not(feature = "tree_walk"))]
type Function = u16;

#[cfg(feature = "tree_walk")]
type Method = Box<crate::compiler::analyser::Method>;

#[cfg(not(feature = "tree_walk"))]
#[derive(Debug, Clone)]
pub struct Method;

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
        value.cloned().into()
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

impl Add for Value {
    type Output = Option<Value>;

    fn add(self, rhs: Self) -> Self::Output {
        &self + &rhs
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

impl Sub for Value {
    type Output = Option<Value>;

    fn sub(self, rhs: Self) -> Self::Output {
        &self - &rhs
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

impl Mul for Value {
    type Output = Option<Value>;

    fn mul(self, rhs: Self) -> Self::Output {
        &self * &rhs
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

impl Div for Value {
    type Output = Option<Value>;

    fn div(self, rhs: Self) -> Self::Output {
        &self / &rhs
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

impl Rem for Value {
    type Output = Option<Value>;

    fn rem(self, rhs: Self) -> Self::Output {
        &self % &rhs
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

impl fmt::Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            #[cfg(feature = "tree_walk")]
            Value::Function(fun) => write!(f, "<function {}>", fun.borrow().name()),
            #[cfg(not(feature = "tree_walk"))]
            Value::Function(_) => write!(f, "<function>"),
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
        } else if s.starts_with('[') && s.ends_with(']') && s.len() > 1 {
            let s = &s[1..s.len() - 1];
            let mut values = vec![];
            for value in s.split(',') {
                values.push(value.parse()?);
            }
            Ok(Value::List(values))
        } else {
            Err(())
        }
    }
}

impl Value {
    pub fn as_number(&self) -> Option<f64> {
        if let Value::Number(n) = self {
            Some(*n)
        } else {
            None
        }
    }

    pub(crate) fn expect_number(&self) -> f64 {
        if let Some(n) = self.as_number() {
            n
        } else {
            internal_error!("called 'expect_number' on {:?}", self)
        }
    }

    pub fn is_something(&self) -> bool {
        !matches!(self, Value::Nothing)
    }

    pub fn as_string(&self) -> Option<&String> {
        if let Value::String(s) = self {
            Some(s)
        } else {
            None
        }
    }

    pub(crate) fn expect_string(&self) -> &String {
        if let Some(s) = self.as_string() {
            s
        } else {
            internal_error!("called 'expect_string' on {:?}", self)
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, EnumCount)]
pub(crate) enum Type {
    Callable,
    Integer,
    List,
    Number,
    String,
}

impl From<u8> for Type {
    fn from(value: u8) -> Self {
        if value as usize >= Type::COUNT {
            internal_error!("cannot interpret '0b{:08b}' as type", value);
        } else {
            // SAFETY: guaranteed by condition
            unsafe { mem::transmute(value) }
        }
    }
}

#[cfg(feature = "compiler")]
mod conversions;
