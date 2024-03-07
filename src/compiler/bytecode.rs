use crate::{
    common::{
        bytecode::{
            Chunk, ExprOpCode, Function, GenericOpCode, Method, Parameters, PatternOpCode, Program,
        },
        discriminant::TARGET_FEATURES,
        expect_option, internal_error,
        value::{FunctionKind, FunctionRef, Value},
        EridaniFunction,
    },
    compiler::ir::{self, pattern},
    prelude::*,
};

use core::{cell::RefCell, fmt};

use alloc::collections::BTreeMap;

struct Functions<'arena> {
    eridani: Vec<Function>,
    native: Vec<(EridaniFunction, String)>,
    references: BTreeMap<&'arena RefCell<ir::Function<'arena>>, (FunctionKind, u16)>,
}

impl<'arena> Functions<'arena> {
    fn new() -> Self {
        Functions {
            eridani: vec![],
            native: vec![],
            references: BTreeMap::new(),
        }
    }
}

impl<'arena> fmt::Debug for Functions<'arena> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        f.debug_struct("Functions")
            .field("eridani", &self.eridani)
            .field(
                "native",
                &self.native.iter().map(|(_, name)| name).collect::<Vec<_>>(),
            )
            .field("references", &self.references)
            .finish()
    }
}

fn compile_pattern(
    pattern: &pattern::Pattern,
    line: usize,
    chunk: &mut Chunk,
    jumps: &mut Vec<usize>,
) {
    match pattern {
        pattern::Pattern::Binary {
            left,
            operator,
            right,
        } => {
            chunk.push_instruction(PatternOpCode::DuplicateValue, line);
            compile_pattern(left, line, chunk, jumps);
            if *operator == pattern::LogOp::Or {
                jumps.push(chunk.push_jump(PatternOpCode::JumpIfTrue, line));
            } else {
                chunk.push_instruction(PatternOpCode::BreakIfFalse, line);
            }
            compile_pattern(right, line, chunk, jumps);
        }
        pattern::Pattern::Concatenation(concatenation) => {
            chunk.push_instruction(PatternOpCode::StartConcat, line);
            let mut var_waiting = false;
            for item in concatenation {
                match item {
                    pattern::Item::Value(value) => {
                        chunk.push_constant_instruction(value.to_owned(), line);
                        if var_waiting {
                            chunk.push_instruction(PatternOpCode::ConcatCompareVarWaiting, line);
                        } else {
                            chunk.push_instruction(PatternOpCode::ConcatCompare, line);
                        }
                    }
                    pattern::Item::Wildcard(variable) => {
                        if variable.pre_set() {
                            chunk.push_variable_width_instruction(
                                GenericOpCode::GetVar,
                                variable.reference(),
                                line,
                            );
                            if var_waiting {
                                chunk
                                    .push_instruction(PatternOpCode::ConcatCompareVarWaiting, line);
                            } else {
                                chunk.push_instruction(PatternOpCode::ConcatCompare, line);
                            }
                        } else {
                            var_waiting = true;
                        }
                    }
                }
            }
            if var_waiting {
                chunk.push_instruction(PatternOpCode::EndConcatWithVar, line);
            } else {
                chunk.push_instruction(PatternOpCode::EndConcat, line);
            }
        }
        pattern::Pattern::List { left, right } => {
            chunk.push_instruction(PatternOpCode::SplitList, line);
            compile_pattern(left, line, chunk, jumps);
            chunk.push_instruction(PatternOpCode::BreakIfFalse, line);
            compile_pattern(right, line, chunk, jumps);
        }
        pattern::Pattern::Literal(value) => {
            chunk.push_constant_instruction(value.to_owned(), line);
            chunk.push_instruction(PatternOpCode::Equal, line);
        }
        pattern::Pattern::OperatorComparison {
            operator_chain,
            comparison,
            rhs,
        } => {
            let mut chain = operator_chain.as_ref().map(|chain| Box::new(chain.clone()));
            while let Some(operator_chain) = chain {
                match operator_chain.rhs {
                    pattern::Item::Value(value) => chunk.push_constant_instruction(value, line),
                    pattern::Item::Wildcard(variable) => chunk.push_variable_width_instruction(
                        GenericOpCode::GetVar,
                        variable.reference(),
                        line,
                    ),
                }
                let operator = match operator_chain.operator {
                    pattern::ArithOp::Add => GenericOpCode::Add,
                    pattern::ArithOp::Sub => GenericOpCode::Subtract,
                    pattern::ArithOp::Mul => GenericOpCode::Multiply,
                    pattern::ArithOp::Div => GenericOpCode::Divide,
                    pattern::ArithOp::Mod => GenericOpCode::Modulo,
                };
                chunk.push_instruction(operator, line);
                chain = operator_chain.next;
            }
            match rhs {
                pattern::Item::Value(value) => {
                    chunk.push_constant_instruction(value.to_owned(), line);
                }
                pattern::Item::Wildcard(variable) => {
                    debug_assert!(variable.pre_set());
                    chunk.push_variable_width_instruction(
                        GenericOpCode::GetVar,
                        variable.reference(),
                        line,
                    );
                }
            }
            let operator = match comparison {
                pattern::Comparision::Equal => PatternOpCode::Equal,
                pattern::Comparision::NotEqual => PatternOpCode::NotEqual,
                pattern::Comparision::Greater => PatternOpCode::Greater,
                pattern::Comparision::GreaterEqual => PatternOpCode::GreaterEqual,
                pattern::Comparision::Less => PatternOpCode::Less,
                pattern::Comparision::LessEqual => PatternOpCode::LessEqual,
            };
            chunk.push_instruction(operator, line);
        }
        pattern::Pattern::Range {
            lower,
            upper,
            inclusive,
        } => {
            chunk.push_constant_instruction(lower.to_owned(), line);
            chunk.push_instruction(PatternOpCode::GreaterEqual, line);
            chunk.push_instruction(PatternOpCode::BreakIfFalse, line);
            chunk.push_constant_instruction(upper.to_owned(), line);
            if *inclusive {
                chunk.push_instruction(PatternOpCode::LessEqual, line);
            } else {
                chunk.push_instruction(PatternOpCode::Less, line);
            }
        }
        pattern::Pattern::Type(kind) => {
            chunk.push_wide_instruction(PatternOpCode::Type, (*kind).into(), line);
        }
        pattern::Pattern::Unary { operator, right } => {
            compile_pattern(right, line, chunk, jumps);
            match operator {
                pattern::UnOp::Not => chunk.push_instruction(PatternOpCode::BreakIfTrue, line),
            }
        }
        pattern::Pattern::Wildcard(variable) => {
            if let Some(variable) = variable {
                if variable.pre_set() {
                    chunk.push_variable_width_instruction(
                        GenericOpCode::GetVar,
                        variable.reference(),
                        line,
                    );
                    chunk.push_instruction(PatternOpCode::Equal, line);
                } else {
                    chunk.push_instruction(PatternOpCode::PushVar, line);
                    chunk.push_constant_instruction(Value::Number(1.0), line);
                }
            } else {
                chunk.push_constant_instruction(Value::Number(1.0), line);
            }
        }
    }
}

fn compile_expr<'arena>(expr: &ir::Expr<'arena>, chunk: &mut Chunk, functions: &Functions<'arena>) {
    match expr {
        ir::Expr::Binary {
            left,
            operator,
            right,
        } => {
            compile_expr(left, chunk, functions);
            compile_expr(right, chunk, functions);
            let op_code = match *operator {
                ir::BinOp::Add => GenericOpCode::Add,
                ir::BinOp::Sub => GenericOpCode::Subtract,
                ir::BinOp::Mul => GenericOpCode::Multiply,
                ir::BinOp::Div => GenericOpCode::Divide,
                ir::BinOp::Mod => GenericOpCode::Modulo,
            };
            chunk.push_instruction(op_code, expr.line());
        }
        ir::Expr::Block { body, .. } => {
            for (i, expr) in body.iter().enumerate() {
                compile_expr(expr, chunk, functions);
                if i < body.len() - 1 {
                    chunk.push_instruction(GenericOpCode::Pop, expr.line());
                }
            }
        }
        ir::Expr::Call {
            callee,
            arguments,
            line,
        } => {
            chunk.push_instruction(ExprOpCode::StartList, *line);
            for argument in arguments {
                compile_expr(argument.expr(), chunk, functions);
                if argument.spread() {
                    chunk.push_instruction(ExprOpCode::Spread, argument.expr().line());
                }
            }
            compile_expr(callee, chunk, functions);
            chunk.push_instruction(ExprOpCode::Call, *line);
        }
        ir::Expr::Function { function, line } => {
            let (kind, reference) = expect_option!(
                functions.references.get(function),
                "function {:?} missing from array",
                function
            );
            chunk.push_constant_instruction(
                Value::Function(FunctionRef {
                    reference: *reference,
                    kind: *kind,
                }),
                *line,
            );
        }
        ir::Expr::Grouping(expr) => compile_expr(expr, chunk, functions),
        ir::Expr::Let { pattern, value } => {
            compile_expr(value, chunk, functions);
            chunk.push_instruction(ExprOpCode::Pattern, value.line());
            let mut jumps = vec![];
            compile_pattern(pattern, value.line(), chunk, &mut jumps);
            chunk.push_instruction(PatternOpCode::EndPattern, value.line());
            chunk.push_constant_instruction(Value::Nothing, value.line());
        }
        ir::Expr::List { expressions, line } => {
            chunk.push_instruction(ExprOpCode::StartList, *line);
            for expression in expressions {
                compile_expr(expression.expr(), chunk, functions);
                if expression.spread() {
                    chunk.push_instruction(ExprOpCode::Spread, expression.expr().line());
                }
            }
            chunk.push_instruction(ExprOpCode::List, *line);
        }
        ir::Expr::ListItem(_) => internal_error!("found list item at compile time"),
        ir::Expr::Literal { value, line } => {
            if value == Value::Nothing {
                chunk.push_instruction(GenericOpCode::Nothing, *line);
            } else {
                chunk.push_constant_instruction(value.to_owned(), *line);
            }
        }
        ir::Expr::Method(method) => {
            let line = method.body().line();
            let method = compile_method(method, functions);
            chunk.push_constant_instruction(Value::Method(Box::new(method)), line);
        }
        ir::Expr::PlaceHolder(_, _) => internal_error!("found placeholder at compile time"),
        ir::Expr::Unary { operator, right } => {
            compile_expr(right, chunk, functions);
            match *operator {
                ir::UnOp::Negate => {
                    chunk.push_instruction(GenericOpCode::Negate, right.line());
                }
            }
        }
        ir::Expr::Variable { reference, line } => {
            chunk.push_variable_width_instruction(GenericOpCode::GetVar, *reference, *line);
        }
    }
}

fn compile_method<'arena>(method: &ir::Method<'arena>, functions: &Functions<'arena>) -> Method {
    let mut chunk = Chunk::new();
    compile_expr(method.body(), &mut chunk, functions);
    chunk.push_instruction(ExprOpCode::Return, method.body().line());

    let mut parameters = Chunk::new();
    for &index in method.arg_order() {
        let (reference, pattern) = &method.args()[index];
        parameters.push_wide_instruction(
            PatternOpCode::HoistValue,
            index as u8,
            method.body().line(),
        );
        let mut jumps = vec![];
        compile_pattern(pattern, method.body().line(), &mut parameters, &mut jumps);
        parameters.push_instruction(PatternOpCode::BreakIfFalse, method.body().line());
        if let Some((reference, pre_set)) = reference {
            if *pre_set {
                parameters.push_variable_width_instruction(
                    GenericOpCode::GetVar,
                    *reference,
                    method.body().line(),
                );
                parameters.push_instruction(PatternOpCode::Equal, method.body().line());
                parameters.push_instruction(PatternOpCode::BreakIfFalse, method.body().line());
                parameters.patch_jumps(jumps, parameters.code().len());
            } else {
                parameters.push_wide_instruction(
                    PatternOpCode::HoistValue,
                    index as u8,
                    method.body().line(),
                );
                parameters.push_instruction(PatternOpCode::PushVar, method.body().line());
                parameters.patch_jumps(jumps, parameters.code().len() - 1);
            }
        } else {
            parameters.patch_jumps(jumps, parameters.code().len());
        }
    }
    parameters.push_instruction(PatternOpCode::PatternSuccess, method.body().line());

    Method::new(chunk, Parameters(parameters), method.args().len())
}

fn compile_function<'arena>(
    methods: &[&'arena RefCell<ir::Method<'arena>>],
    name: String,
    functions: &Functions<'arena>,
) -> Function {
    let mut compiled_methods = Vec::with_capacity(methods.len());
    for method in methods {
        let method = compile_method(&method.borrow(), functions);
        compiled_methods.push(method);
    }

    Function::new(compiled_methods, name)
}

pub(super) fn compile(analysed: &ir::Program) -> Program {
    let mut functions = Functions::new();

    let mut num_eridani = 0;
    let mut num_native = 0;
    for &function in analysed.functions() {
        let function_ref = function.borrow();
        match &*function_ref {
            ir::Function::Eridani { .. } => {
                drop(function_ref);
                functions
                    .references
                    .insert(function, (FunctionKind::Eridani, num_eridani));
                num_eridani += 1;
            }
            ir::Function::Rust { name, func } => {
                functions.native.push((*func, name.to_owned()));
                drop(function_ref);
                functions
                    .references
                    .insert(function, (FunctionKind::Native, num_native));
                num_native += 1;
            }
        }
    }
    functions.eridani = Vec::with_capacity(num_eridani.into());

    for &function in analysed.functions() {
        if let ir::Function::Eridani { methods, name } = &*function.borrow() {
            functions
                .eridani
                .push(compile_function(methods, name.to_owned(), &functions));
        }
    }

    let (_, entry_point) = expect_option!(functions.references.get(&analysed.entry_point()));

    let Functions {
        eridani,
        native,
        references: _,
    } = functions;

    Program {
        functions: eridani,
        natives: native,
        entry_point: *entry_point,
        features: TARGET_FEATURES,
    }
}
