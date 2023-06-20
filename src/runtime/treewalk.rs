use crate::{
    common::{internal_error, value::Value, EridaniFunction},
    compiler::analyser::{match_args, BinOp, Expr, Function, ListExpr, MatchResult, Program, UnOp},
    runtime::{EridaniResult, Error, Result},
};

use alloc::{collections::VecDeque, rc::Rc};
use core::{cell::RefCell, cmp::Ordering};

fn expr(
    expression: Expr,
    variables: Vec<Value>,
    function_name: &str,
) -> Result<(Value, Vec<Value>)> {
    let line = expression.line();
    match expression {
        Expr::Binary {
            left,
            operator,
            right,
        } => {
            let (left, variables) = expr(*left, variables, function_name)?;
            let (right, variables) = expr(*right, variables, function_name)?;

            let result = match operator {
                BinOp::Add => &left + &right,
                BinOp::Sub => &left - &right,
                BinOp::Mul => &left * &right,
                BinOp::Div => &left / &right,
                BinOp::Mod => &left % &right,
            };

            if let Some(value) = result {
                Ok((value, variables))
            } else {
                let message =
                    format!("Operation {operator} not permitted between {left} and {right}");
                Err(Error::new("Arithmetic", &message, line))
            }
        }
        Expr::Block { body, .. } => {
            let mut variables = variables;
            let mut value = Value::Nothing;
            for expression in body {
                let (new_value, new_variables) = expr(expression, variables, function_name)?;
                variables = new_variables;
                value = new_value;
            }
            Ok((value, variables))
        }
        Expr::Call {
            callee,
            arguments,
            line,
        } => {
            let (callee, mut variables) = expr(*callee, variables, function_name)?;

            let mut evaluated_arguments = vec![];
            for argument in arguments {
                let (values, new_variables) = list_expr(argument, variables, function_name, line)?;
                evaluated_arguments.extend(values);
                variables = new_variables;
            }
            let arguments = evaluated_arguments;

            match callee {
                Value::Function(fun) => {
                    let name = fun.borrow().name().to_owned();
                    Ok((
                        function(fun, &arguments, &name, line)
                            .init_or_add_context(function_name, line)?,
                        variables,
                    ))
                }
                Value::Method(method) => {
                    let new_variables =
                        match match_args(method.args(), &arguments, method.arg_order()) {
                            MatchResult::Success(variables) => variables,
                            MatchResult::Fail => {
                                let message =
                                    format!("Method does not match the arguments {:?}", arguments);
                                return Err(Error::new("Match", &message, line));
                            }
                            MatchResult::Error => {
                                return Err(Error::new(
                                    "Pattern",
                                    "Circular dependencies in method arguments",
                                    line,
                                ))
                            }
                            MatchResult::Indeterminable => {
                                internal_error!("got `Indeterminable` from `match_args`")
                            }
                        };

                    Ok((
                        expr(method.body().to_owned(), new_variables, function_name)?.0,
                        variables,
                    ))
                }
                _ => {
                    let message = format!("Cannot call value {callee}");
                    Err(Error::new("Type", &message, line))
                }
            }
        }
        Expr::Function { function, .. } => Ok((Value::Function(function), variables)),
        Expr::Grouping(expression) => expr(*expression, variables, function_name),
        Expr::Let { pattern, value } => {
            let (value, variables) = expr(*value, variables, function_name)?;
            let variables = match pattern.matches(&value, variables) {
                Some(variables) => variables,
                None => {
                    let message = format!("Pattern {:?} does not match {value}", pattern);
                    return Err(Error::new("Match", &message, line));
                }
            };

            Ok((Value::Nothing, variables))
        }
        Expr::List { expressions, .. } => {
            let mut new_variables = variables.clone();
            let expressions: Result<Vec<Vec<Value>>> = expressions
                .into_iter()
                .map(|expression| {
                    let (values, vars) =
                        list_expr(expression, new_variables.clone(), function_name, line)?;
                    new_variables = vars;
                    Ok(values)
                })
                .collect();
            let expressions = expressions?.into_iter().flatten().collect();
            Ok((Value::List(expressions), variables))
        }
        Expr::ListItem(item) => {
            if let Expr::Unary { operator, .. } = item.expr() {
                let message = format!("Invalid use of '{operator}'");
                Err(Error::new("Syntax", &message, line))
            } else {
                internal_error!("")
            }
        }
        Expr::Literal { value, .. } => Ok((value, variables)),
        Expr::Method(method) => Ok((Value::Method(method), variables)),
        Expr::PlaceHolder(placeholder, module) => {
            internal_error!(
                "placeholder '{:?}' in '{:?}' at runtime",
                placeholder,
                module
            )
        }
        Expr::Unary { operator, right } => {
            let (right, variables) = expr(*right, variables, function_name)?;
            match operator {
                UnOp::Negate => {
                    if let Some(value) = -&right {
                        Ok((value, variables))
                    } else {
                        let message = format!("Cannot negate '{}'", right);
                        Err(Error::new("Type", &message, line))
                    }
                }
            }
        }
        Expr::Variable { reference, .. } => {
            if let Some(value) = variables.get(reference as usize) {
                Ok((value.to_owned(), variables))
            } else {
                internal_error!("unrecognised variable reference '{}'", reference)
            }
        }
    }
}

fn list_expr(
    expression: ListExpr,
    variables: Vec<Value>,
    function_name: &str,
    line: usize,
) -> Result<(Vec<Value>, Vec<Value>)> {
    let spread = expression.spread();
    let (expression, variables) = expr(expression.expr(), variables, function_name)?;

    if spread {
        let list = if let Value::List(list) = expression {
            list
        } else {
            return Err(Error::new("Type", "Cannot use '..' on a non-list", line));
        };

        Ok((list, variables))
    } else {
        Ok((vec![expression], variables))
    }
}

fn native_function(
    mut function: Box<dyn EridaniFunction>,
    function_name: &str,
    args: &[Value],
) -> Result<Value> {
    match function(args) {
        Ok(value) => Ok(value),
        Err(error) => Err(Error::from_argument_error(error, function_name)),
    }
}

fn function(
    function: Rc<RefCell<Function>>,
    args: &[Value],
    function_name: &str,
    line: usize,
) -> Result<Value> {
    if let Some(native) = function.borrow().native() {
        return native_function(native, function.borrow().name(), args);
    }

    let matches: Option<Vec<_>> = function
        .borrow()
        .methods()
        .iter()
        .map(|method| match_args(method.borrow().args(), args, method.borrow().arg_order()))
        .collect();

    let matches = match matches {
        Some(value) => value,
        None => {
            return Err(Error::new(
                "Pattern",
                "Circular dependencies in method arguments",
                line,
            ))
        }
    };

    let mut matches: Vec<_> = matches
        .into_iter()
        .enumerate()
        .filter(|(_, values)| values.is_some())
        .map(|(index, values)| (index, values.unwrap()))
        .collect();

    if matches.is_empty() {
        let mut args_buf = String::with_capacity(args.len() * 6);
        for (i, arg) in args.iter().enumerate() {
            args_buf.push_str(&format!("{arg}"));
            if i + 1 < args.len() {
                args_buf.push_str(", ");
            }
        }
        let message = format!(
            "Function '{}' has no methods that match the arguments ({})",
            function.borrow().name(),
            args_buf
        );
        Err(Error::new("Match", &message, line)).extend_trace(function_name, line)
    } else if matches.len() == 1 {
        let (function_index, bindings) = VecDeque::from(matches).pop_front().unwrap();
        Ok(expr(
            function.borrow().methods()[function_index]
                .borrow()
                .body()
                .to_owned(),
            bindings,
            function.borrow().name(),
        )?
        .0)
    } else {
        let precedences: Vec<_> = matches
            .iter()
            .map(|(index, bindings)| {
                function.borrow().methods()[*index].borrow().precedence(
                    &bindings
                        .iter()
                        .enumerate()
                        .map(|(a, _)| a as u16)
                        .collect::<Vec<_>>(),
                )
            })
            .collect();

        let mut min_precedence = precedences[0];
        let mut min_index = 0;

        for (i, precedence) in precedences[1..].iter().enumerate() {
            match precedence.cmp(&min_precedence) {
                Ordering::Less => {
                    min_precedence = *precedence;
                    min_index = i;
                }
                Ordering::Equal => return Err(Error::new("Match", "Method collision", line)),
                _ => {}
            }
        }

        let (function_index, variables) = matches.remove(min_index);
        Ok(expr(
            function.borrow().methods()[function_index]
                .borrow()
                .body()
                .to_owned(),
            variables,
            function.borrow().name(),
        )?
        .0)
    }
}

pub fn walk_tree(program: Program, args: &[Value]) -> Result<Value> {
    let entry_point = Rc::clone(program.entry_point());

    function(entry_point, args, "<program>", 0)
}
