use crate::{
    common::{internal_error, match_engine::match_args, value::Value, EridaniFunction},
    compiler::analyser::{BinOp, Expr, Function, Program, UnOp},
    prelude::*,
    runtime::{EridaniResult, Error, Result},
};

use alloc::{collections::BTreeMap, rc::Rc};
use core::cell::RefCell;

fn expr(expression: Expr, bindings: &mut BTreeMap<String, Value>, line: usize) -> Result<Value> {
    match expression {
        Expr::Binary {
            left,
            operator,
            right,
        } => {
            let left = expr(*left, bindings, line)?;
            let right = expr(*right, bindings, line)?;

            let result = match operator {
                BinOp::Add => &left + &right,
                BinOp::Sub => &left - &right,
                BinOp::Mul => &left * &right,
                BinOp::Div => &left / &right,
                BinOp::Mod => &left % &right,
            };

            if let Some(value) = result {
                Ok(value)
            } else {
                let message =
                    format!("Operation {operator} not permitted between {left} and {right}");
                Err(Error::new("Arithmetic", &message, line))
            }
        }
        Expr::Block { body, .. } => {
            let values: Result<Vec<Value>> = body
                .into_iter()
                .map(|expression| expr(expression, bindings, line))
                .collect();
            Ok(values?.into_iter().last().unwrap_or(Value::Nothing))
        }
        Expr::Call {
            callee,
            arguments,
            line,
        } => {
            let callee = expr(*callee, bindings, line)?;
            let arguments: Result<Vec<Value>> = arguments
                .into_iter()
                .map(|expression| expr(expression, bindings, line))
                .collect();
            let arguments = arguments?;
            match callee {
                Value::Function(fun) => function(fun, &arguments, line),
                Value::Method(method) => {
                    let mut internal_bindings = match match_args(method.args(), &arguments) {
                        Some(bindings) => bindings,
                        None => {
                            let message =
                                format!("Method does not match the arguments {:?}", arguments);
                            return Err(Error::new("Match", &message, line));
                        }
                    };

                    expr(method.body().to_owned(), &mut internal_bindings, line)
                }
                _ => {
                    let message = format!("Cannot call value {callee}");
                    Err(Error::new("Type", &message, line))
                }
            }
        }
        Expr::Function(func) => Ok(Value::Function(func)),
        Expr::Grouping(expression) => expr(*expression, bindings, line),
        Expr::Let { pattern, value } => {
            let value = expr(*value, bindings, line)?;
            let mut new_bindings = BTreeMap::new();
            match pattern.matches(&value, &mut new_bindings) {
                Some(_) => {}
                None => {
                    let message = format!("Pattern {:?} does not match {value}", pattern);
                    return Err(Error::new("Match", &message, line));
                }
            }

            Ok(Value::Nothing)
        }
        Expr::List { expressions, line } => {
            let expressions: Result<Vec<Value>> = expressions
                .into_iter()
                .map(|expression| expr(expression, bindings, line))
                .collect();
            let expressions = expressions?;
            Ok(Value::List(expressions))
        }
        Expr::Literal { value, .. } => Ok(value),
        Expr::Method(method) => Ok(Value::Method(method)),
        Expr::PlaceHolder(placeholder) => {
            internal_error!("placeholder '{:?}' at runtime", placeholder)
        }
        Expr::Unary { operator, right } => {
            let right = expr(*right, bindings, line)?;
            match operator {
                UnOp::Negate => {
                    if let Some(value) = -right {
                        Ok(value)
                    } else {
                        todo!()
                    }
                }
            }
        }
        Expr::Variable(name) => {
            if let Some(value) = bindings.get(&name) {
                Ok(value.to_owned())
            } else {
                let message = format!("No name {name} in scope");
                Err(Error::new("Name", &message, line))
            }
        }
    }
}

fn native_function(
    function: Box<dyn EridaniFunction>,
    function_name: &str,
    args: &[Value],
) -> Result<Value> {
    match function(args) {
        Ok(value) => Ok(value),
        Err(error) => Err(Error::from_argument_error(error, function_name)),
    }
}

fn function(function: Rc<RefCell<Function>>, args: &[Value], line: usize) -> Result<Value> {
    if let Some(native) = function.borrow().native() {
        return native_function(native, function.borrow().name(), args);
    }

    let mut matches: Vec<_> = function
        .borrow()
        .methods()
        .iter()
        .enumerate()
        .flat_map(|(i, method)| {
            match_args(method.borrow().args(), args).map(|bindings| (i, bindings))
        })
        .collect();

    if matches.is_empty() {
        let message = format!(
            "Function '{}' has no methods that match the arguments {:?}",
            function.borrow().name(),
            args
        );
        Err(Error::new("Match", &message, line)).extend_trace(function.borrow().name(), 0)
    } else if matches.len() == 1 {
        expr(
            function.borrow().methods()[0].borrow().body().to_owned(),
            &mut matches[0].1,
            line,
        )
    } else {
        todo!()
    }
}

pub fn walk_tree(program: Program, args: &[Value]) -> Result<Value> {
    let (entry_point, _) = program;

    function(entry_point, args, 0)
}
