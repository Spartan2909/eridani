use crate::{
    common::{
        value::{Pattern, Value},
        EridaniFunction,
    },
    compiler::{
        internal_error,
        parser::{self, ParseTree},
        scanner::TokenType,
        Result,
    },
    prelude::*,
};

use core::{cell::RefCell, fmt::Debug};

pub enum Function {
    Eridani {
        name: String,
        methods: Vec<Method>,
    },
    Rust {
        name: String,
        func: Box<dyn EridaniFunction>,
    },
}

impl Clone for Function {
    fn clone(&self) -> Self {
        match self {
            Function::Eridani { name, methods } => Function::Eridani {
                name: name.clone(),
                methods: methods.clone(),
            },
            Function::Rust { name, func } => Function::Rust {
                name: name.clone(),
                func: func.clone_box(),
            },
        }
    }
}

impl Debug for Function {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        match self {
            Function::Rust { name, .. } => {
                write!(f, "Function::Rust {{\n    name: \"{name}\",\n}}")
            }
            Function::Eridani { name, .. } => {
                write!(f, "Function::Eridani {{\n    name: \"{name}\",\n}}")
            }
        }
    }
}

#[derive(Debug, Clone)]
pub struct Method {
    args: Vec<(Option<String>, Pattern)>,
    body: Expr,
}

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
    Literal {
        value: Value,
        line: usize,
    },
    Method(Box<Method>),
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
            ParseExpr::Let { pattern, value } => Self::Let {
                pattern: pattern.into(),
                value: Box::new((&*value).into()),
            },
            ParseExpr::List { expressions, end } => Self::List {
                expressions: vec_convert(expressions),
                line: end.line(),
            },
            ParseExpr::Literal(token) => {
                let line = token.line();
                Self::Literal {
                    value: token.into(),
                    line,
                }
            }
            ParseExpr::Method(method) => {
                let args: Vec<(Option<String>, Pattern)> = method
                    .args()
                    .iter()
                    .map(|x| (x.name(), x.pattern().into()))
                    .collect();
                let body: Expr = method.body().into();
                Expr::Method(Box::new(Method { args, body }))
            }
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

struct Module {
    submodules: Vec<Rc<RefCell<Module>>>,
    functions: Vec<Rc<RefCell<Function>>>,
}

struct Analyser {
    modules: Vec<Rc<RefCell<Module>>>,
    current_module: Rc<RefCell<Module>>,
}

pub fn analyse(parse_tree: ParseTree, entry_point: &str) -> Result<Vec<Rc<Function>>> {
    todo!()
}
