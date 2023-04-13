use crate::prelude::*;

#[derive(Debug, Clone)]
pub enum Value {
    Number(f64),
    String(String),
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
mod conversions;
