use crate::{common::internal_error, prelude::*};

use alloc::{collections::BTreeMap, rc::Rc};
use core::{
    cell::RefCell,
    cmp::Ordering,
    fmt,
    ops::{Add, Div, Mul, Neg, Rem, Sub},
};

#[derive(Debug, Clone)]
pub enum Value {
    Function(Function),
    List(Vec<Value>),
    Nothing,
    Method(Method),
    Number(f64),
    String(String),
}

#[cfg(feature = "tree_walk")]
type Function = Rc<RefCell<crate::compiler::analyser::Function>>;

#[cfg(feature = "tree_walk")]
type Method = Box<crate::compiler::analyser::Method>;

#[cfg(not(feature = "tree_walk"))]
struct Method;

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

    pub fn number(&self) -> Option<f64> {
        if let Value::Number(n) = self {
            Some(*n)
        } else {
            None
        }
    }

    pub fn expect_number(&self) -> f64 {
        if let Some(n) = self.number() {
            n
        } else {
            internal_error!("called 'expect_number' on {}", self)
        }
    }

    pub fn is_something(&self) -> bool {
        !matches!(self, Value::Nothing)
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
                if value.is_kind(*kind) {
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

    pub fn compare_specicifity(&self, other: &Pattern, bindings: &[Option<String>]) -> Ordering {
        match self {
            Pattern::Binary {
                left,
                operator,
                right,
            } => {
                if *operator == LogOp::And {
                    let most_specific =
                        if left.compare_specicifity(right, bindings) == Ordering::Greater {
                            left
                        } else {
                            right
                        };

                    let mut comparison = most_specific.compare_specicifity(other, bindings);
                    if comparison == Ordering::Equal {
                        comparison = Ordering::Greater
                    }

                    comparison
                } else {
                    let least_specific =
                        if left.compare_specicifity(right, bindings) == Ordering::Less {
                            left
                        } else {
                            right
                        };

                    let mut comparison = least_specific.compare_specicifity(other, bindings);
                    if comparison == Ordering::Equal {
                        comparison = Ordering::Less
                    }

                    comparison
                }
            }
            Pattern::Comparision { .. } => {
                if let Pattern::Comparision { .. } = other {
                    Ordering::Equal
                } else if other.is_type() || other.is_wildcard(bindings) {
                    Ordering::Greater
                } else if other.is_binary() {
                    other.compare_specicifity(self, bindings).reverse()
                } else {
                    Ordering::Less
                }
            }
            Pattern::List {
                left: left1,
                right: right1,
            } => {
                if let Pattern::List {
                    left: left2,
                    right: right2,
                } = other
                {
                    let left_comp = left1.compare_specicifity(left2, bindings);
                    let right_comp = right1.compare_specicifity(right2, bindings);

                    if left_comp == right_comp {
                        left_comp
                    } else {
                        Ordering::Equal
                    }
                } else if other.is_literal(bindings) {
                    Ordering::Less
                } else if other.is_binary() {
                    other.compare_specicifity(self, bindings).reverse()
                } else {
                    Ordering::Greater
                }
            }
            Pattern::Literal(_) => {
                if other.is_literal(bindings) {
                    Ordering::Equal
                } else if other.is_binary() {
                    other.compare_specicifity(self, bindings).reverse()
                } else {
                    Ordering::Greater
                }
            }
            Pattern::OperatorComparison { .. } => {
                if other.is_operator_comparison() {
                    Ordering::Equal
                } else if other.is_binary() {
                    other.compare_specicifity(self, bindings).reverse()
                } else if other.is_comparison() || other.is_type() || other.is_wildcard(bindings) {
                    Ordering::Greater
                } else {
                    Ordering::Less
                }
            }
            Pattern::Range {
                lower: lower1,
                upper: upper1,
                inclusive: inclusive1,
            } => {
                if let Pattern::Range {
                    lower: lower2,
                    upper: upper2,
                    inclusive: inclusive2,
                } = other
                {
                    let lower1 = lower1.expect_number();
                    let upper1 = upper1.expect_number();
                    let difference1 = if *inclusive1 {
                        upper1 - lower1 + 1.0
                    } else {
                        upper1 - lower1
                    };

                    let lower2 = lower2.expect_number();
                    let upper2 = upper2.expect_number();
                    let difference2 = if *inclusive2 {
                        upper2 - lower2 + 1.0
                    } else {
                        upper2 - lower2
                    };

                    if let Some(cmp) = difference1.partial_cmp(&difference2) {
                        cmp
                    } else {
                        internal_error!("NaN in range")
                    }
                } else if other.is_binary() {
                    other.compare_specicifity(self, bindings).reverse()
                } else if other.is_literal(bindings) || other.is_list() {
                    Ordering::Less
                } else {
                    Ordering::Greater
                }
            }
            Pattern::Type(_) => {
                if other.is_type() {
                    Ordering::Equal
                } else if other.is_binary() {
                    other.compare_specicifity(self, bindings).reverse()
                } else if other.is_wildcard(bindings) {
                    Ordering::Greater
                } else {
                    Ordering::Less
                }
            }
            Pattern::Unary { operator, right } => match *operator {
                UnOp::Not => right.compare_specicifity(other, bindings).reverse(),
            },
            Pattern::Wildcard(name) => {
                if bindings.contains(name) {
                    Pattern::Literal(Value::Nothing).compare_specicifity(other, bindings)
                } else if other.is_wildcard(bindings) {
                    Ordering::Equal
                } else {
                    Ordering::Less
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

#[cfg(feature = "compiler")]
mod conversions;
