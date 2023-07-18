use crate::{
    common::value::Value,
    compiler::{
        ir::{match_engine::partial_match, Expr, Program},
        Error, Result,
    },
};

fn fold_constants(body: &mut Expr) -> Result<()> {
    for _ in 0..3 {
        match body {
            Expr::Binary {
                left,
                operator,
                right,
            } => match (left.as_ref(), right.as_ref()) {
                (Expr::Literal { value: left, .. }, Expr::Literal { value: right, .. }) => {
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
                _ => {}
            },
            Expr::Unary { operator, right } => {
                if let Expr::Literal { value, .. } = right.as_ref() {
                    if let Some(value) = operator.operate(value) {
                        *body = Expr::Literal {
                            value,
                            line: body.line(),
                        };
                    } else {
                        let message =
                            format!("Operator '{}' not permitted on '{}'", operator, value);
                        return Err(Error::new(body.line(), "Type", "", &message));
                    }
                }
            }
            _ => {}
        }
    }

    Ok(())
}

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
                    let mut matches = vec![];
                    for method in function.borrow().methods().unwrap() {
                        if partial_match(
                            method.borrow().args(),
                            &arguments,
                            method.borrow().arg_order(),
                        )
                        .is_ok()
                        {
                            matches.push(method);
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
