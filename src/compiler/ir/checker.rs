use crate::{
    common::internal_error,
    compiler::{
        ir::{Expr, Item, LogOp, Pattern, Program, Variable},
        Error, Result,
    },
};

pub(super) fn verify_pattern(pattern: &Pattern, line: usize, resolved_names: &[u16]) -> Result<()> {
    match pattern {
        Pattern::Binary {
            left,
            right,
            operator,
        } => {
            if *operator == LogOp::And && (left.is_literal(&[]) || right.is_literal(&[])) {
                return Err(Error::new(
                    line,
                    "Pattern",
                    "",
                    "Patterns of the form '<literal> & <pattern>' are forbidden",
                ));
            } else if *operator == LogOp::Or
                && (!left.references().is_empty() || !right.references().is_empty())
            {
                return Err(Error::new(
                    line,
                    "Pattern",
                    "",
                    "'|' patterns cannot assign to variables",
                ));
            }

            verify_pattern(left, line, resolved_names)?;
            verify_pattern(right, line, resolved_names)?;
        }
        Pattern::Concatenation(items) => {
            let mut wildcard = false;

            for item in items {
                if let Item::Wildcard(var) = item {
                    if resolved_names.contains(&var.reference()) {
                        wildcard = false;
                    } else {
                        if wildcard {
                            return Err(Error::new(
                                line,
                                "Pattern",
                                "",
                                "Sequential wildcards in concatenations are not allowed",
                            ));
                        }
                        wildcard = true;
                    }
                } else {
                    wildcard = false;
                }
            }
        }
        Pattern::List { left, right } => {
            verify_pattern(left, line, resolved_names)?;
            verify_pattern(right, line, resolved_names)?;
        }
        Pattern::OperatorComparison {
            rhs: Item::Wildcard(variable),
            ..
        } => {
            if !variable.pre_set() {
                return Err(Error::new(
                    line,
                    "Pattern",
                    "",
                    "Cannot compare to a wildcard",
                ));
            }
        }
        Pattern::Range {
            lower,
            upper,
            inclusive,
        } => {
            let lower = lower.expect_number();
            let upper = upper.expect_number();
            if lower > upper || !inclusive && (lower - upper).abs() < f64::EPSILON {
                return Err(Error::new(line, "Pattern", "", "Range will never match"));
            }
        }
        Pattern::Wildcard(Some(Variable::Name(name))) => {
            internal_error!("unbound wildcard pattern: '{}'", name);
        }
        _ => {}
    }

    Ok(())
}

fn check_for_list_item(expr: &Expr) -> Result<()> {
    match expr {
        Expr::Binary { left, right, .. } => {
            check_for_list_item(left)?;
            check_for_list_item(right)?;
        }
        Expr::Block { body, .. } => {
            for expr in body {
                check_for_list_item(expr)?;
            }
        }
        Expr::Grouping(expr) => check_for_list_item(expr)?,
        Expr::ListItem(expr) => {
            if expr.spread {
                return Err(Error::new(
                    expr.expr.line(),
                    "Syntax",
                    "",
                    "Cannot use '..' in this context",
                ));
            }
        }
        _ => {}
    }

    Ok(())
}

pub(super) fn check_before_optimisation(converted: Program) -> Result<Program> {
    for function in converted.functions() {
        if let Some(methods) = function.borrow().methods() {
            for method in methods {
                check_for_list_item(method.borrow().body())?;
            }
        }
    }
    Ok(converted)
}
