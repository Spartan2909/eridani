use super::{parser, scanner::TokenType, Error, Result};

use crate::common::value::Value;

#[derive(Debug, Clone)]
pub struct Ast {}

#[derive(Debug, Clone, Copy)]
pub enum BinOp {
    Add,
    Sub,
    Mul,
    Div,
    Mod,
}

#[derive(Debug, Clone, Copy)]
pub enum UnOp {
    Negate,
}

fn vec_convert(value: Vec<parser::Expr>) -> Result<Vec<Expr>> {
    value
        .into_iter()
        .map(|x| x.try_into())
        .collect::<Result<Vec<Self>>>()
}

#[derive(Debug, Clone)]
pub enum Expr {
    Binary {
        left: Box<Self>,
        operator: BinOp,
        right: Box<Self>,
    },
    Block {
        body: Vec<Self>,
        end: Token,
    },
    Call {
        callee: Box<Self>,
        arguments: Vec<Self>,
        line: usize,
    },
    Grouping(Box<Self>),
    Let {
        pattern: Pattern,
        value: Box<Self>,
    },
    List {
        expressions: Vec<Self>,
        line: usize,
    },
    Literal(Value),
    Method(Box<Method>),
    Unary {
        operator: UnOp,
        right: Box<Self>,
    },
    Variable(ImportTree),
}

impl TryFrom<parser::Expr> for Expr {
    type Error = Error;

    fn try_from(value: parser::Expr) -> Result<Self> {
        use parser::Expr as ParseExpr;

        match value {
            ParseExpr::Binary {
                left,
                operator,
                right,
            } => {
                let operator = match operator.kind() {
                    TokenType::Plus => BinOp::Add,
                    TokenType::Minus => BinOp::Sub,
                    TokenType::Star => BinOp::Mul,
                    TokenType::Slash => BinOp::Div,
                    TokenType::Mod => BinOp::Mod,
                    _ => panic!(
                        "internal compiler error: parsed token '{:?}' as operator",
                        operator
                    ),
                };

                Ok(Self::Binary {
                    left: Box::new((&*left).try_into()?),
                    operator,
                    right: Box::new((&*right).try_into()?),
                })
            }
            ParseExpr::Block { body, end } => Ok(Self::Block {
                body: vec_convert(body)?,
                end: end.line(),
            }),
            ParseExpr::Call {
                callee,
                paren,
                arguments,
            } => Ok(Self::Call {
                callee: Box::new((&*callee).try_into()?),
                arguments: vec_convert(arguments)?,
                line: paren.line(),
            }),
            ParseExpr::Grouping(expr) => Ok(Self::Grouping(Box::new((&*expr).try_into()?))),
            ParseExpr::Let { pattern, value } => todo!(),
            ParseExpr::List { expressions, end } => Ok(Self::List {
                expressions: vec_convert(expressions)?,
                line: end.line(),
            }),
        }
    }
}

impl TryFrom<&parser::Expr> for Expr {
    type Error = Error;

    fn try_from(value: &parser::Expr) -> core::result::Result<Self, Self::Error> {
        value.clone().try_into()
    }
}
