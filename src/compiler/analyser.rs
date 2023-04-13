use crate::{
    common::{Pattern, Value},
    compiler::{
        internal_error,
        parser::{self, ParseTree},
        scanner::TokenType,
        Result,
    },
    prelude::*,
};

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

fn vec_convert(value: Vec<parser::Expr>) -> Vec<Expr> {
    value.into_iter().map(|x| x.into()).collect::<Vec<Expr>>()
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
        line: usize,
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
    //Method(Box<Method>),
    Unary {
        operator: UnOp,
        right: Box<Self>,
    },
    //Variable(ImportTree),
}

impl From<parser::Expr> for Expr {
    fn from(value: parser::Expr) -> Self {
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
                    _ => internal_error!("parsed token '{:?}' as operator", operator),
                };

                Self::Binary {
                    left: Box::new((&*left).into()),
                    operator,
                    right: Box::new((&*right).into()),
                }
            }
            ParseExpr::Block { body, end } => Self::Block {
                body: vec_convert(body),
                line: end.line(),
            },
            ParseExpr::Call {
                callee,
                paren,
                arguments,
            } => Self::Call {
                callee: Box::new((&*callee).into()),
                arguments: vec_convert(arguments),
                line: paren.line(),
            },
            ParseExpr::Grouping(expr) => Self::Grouping(Box::new((&*expr).into())),
            ParseExpr::Let { pattern, value } => todo!(),
            ParseExpr::List { expressions, end } => Self::List {
                expressions: vec_convert(expressions),
                line: end.line(),
            },
            ParseExpr::Literal(token) => Self::Literal(token.into()),
            ParseExpr::Method(method) => todo!(),
            ParseExpr::Unary { operator, right } => {
                let operator = match operator.kind() {
                    TokenType::Minus => UnOp::Negate,
                    _ => internal_error!("parsed token {:?} as unary", operator),
                };

                Self::Unary {
                    operator,
                    right: Box::new((&*right).into()),
                }
            }
            ParseExpr::Variable(var) => todo!(),
        }
    }
}

impl From<&parser::Expr> for Expr {
    fn from(value: &parser::Expr) -> Self {
        value.clone().into()
    }
}

pub fn analyse(_parse_tree: ParseTree) -> Result<Ast> {
    todo!()
}
