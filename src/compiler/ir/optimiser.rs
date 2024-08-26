use crate::{
    common::value::Value,
    compiler::{
        ir::{match_engine::partial_match, Expr, Program},
        Error, Result,
    },
    prelude::*,
};

use super::match_engine::MatchResult;

use alloc::rc::Rc;

fn fold_constants(body: &mut Expr) -> Result<()> {
    match body {
        Expr::Binary {
            left,
            operator,
            right,
        } => {
            fold_constants(left)?;
            fold_constants(right)?;

            if let (Expr::Literal { value: left, .. }, Expr::Literal { value: right, .. }) =
                (&**left, &**right)
            {
                if let Some(value) = operator.operate(left, right) {
                    *body = Expr::Literal {
                        value,
                        line: body.line(),
                    };
                } else {
                    let message = format!(
                        "Operation '{operator}' not permitted between '{left}' and '{right}'"
                    );
                    return Err(Error::new(body.line(), "Arithmetic", "", &message));
                }
            }
        }
        Expr::Unary { operator, right } => {
            fold_constants(right)?;

            if let Expr::Literal { value, .. } = &**right {
                if let Some(value) = operator.operate(value) {
                    *body = Expr::Literal {
                        value,
                        line: body.line(),
                    };
                } else {
                    let message = format!("Operator '{operator}' not permitted on '{value}'");
                    return Err(Error::new(body.line(), "Type", "", &message));
                }
            }
        }
        _ => {}
    }

    Ok(())
}

#[allow(clippy::single_match)]
fn identify_methods(body: &mut Expr) -> Result<()> {
    match body {
        Expr::Call {
            callee, arguments, ..
        } => {
            let arguments: Vec<Option<Value>> = arguments
                .iter()
                .map(|arg| {
                    if let Expr::Literal { value, .. } = &arg.expr {
                        Some(value.to_owned())
                    } else {
                        None
                    }
                })
                .collect();
            match callee.as_ref() {
                Expr::Function { function, .. } if function.borrow().methods().is_some() => {
                    let mut successes = vec![];
                    let mut indeterminable = vec![];
                    for method in function.borrow().methods().unwrap() {
                        let match_result = partial_match(
                            method.borrow().args(),
                            &arguments,
                            method.borrow().arg_order(),
                        );
                        match match_result {
                            MatchResult::Success(_) => successes.push(Rc::clone(method)),
                            MatchResult::Indeterminable => indeterminable.push(Rc::clone(method)),
                            MatchResult::Fail => {}
                        }
                    }
                    if let Some(method) = successes.pop() {
                        if successes.is_empty() {
                            *callee = Box::new(Expr::Method(Box::new(method.borrow().clone())));
                        }
                    } else if let Some(method) = indeterminable.pop() {
                        if indeterminable.is_empty() {
                            *callee = Box::new(Expr::Method(Box::new(method.borrow().clone())));
                        }
                    }
                }
                Expr::Method(method) => {
                    if !partial_match(method.args(), &arguments, method.arg_order()).is_ok() {
                        return Err(Error::new(
                            body.line(),
                            "Match",
                            "",
                            "Pattern cannot match given arguments",
                        ));
                    }
                }
                _ => {}
            }
        }
        _ => {}
    }

    Ok(())
}

pub(super) fn optimise(checked: Program) -> Result<Program> {
    for function in checked.functions() {
        if let Some(methods) = function.borrow().methods() {
            for method in methods {
                let mut method = method.borrow_mut();
                fold_constants(&mut method.body)?;
                identify_methods(&mut method.body)?;
            }
        }
    }

    Ok(checked)
}
