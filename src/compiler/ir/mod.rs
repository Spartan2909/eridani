mod checker;
mod converter;
mod match_engine;
mod modules;
mod optimiser;
pub(super) mod pattern;

use modules::Module;

use crate::{
    common::{internal_error, value::Value, EridaniFunction},
    compiler::{
        arena::Arena,
        ir::pattern::{Item, LogOp, Pattern, Variable},
        parser::{ImportTree, ParseTree},
        Result,
    },
    prelude::*,
};

use core::{cell::RefCell, cmp::Ordering, fmt, ptr::NonNull};

use alloc::collections::BTreeMap;

use bimap::BiMap;

pub(crate) struct Program<'arena> {
    entry_point: &'arena RefCell<Function<'arena>>,
    functions: Vec<&'arena RefCell<Function<'arena>>>,
}

impl<'arena> Program<'arena> {
    fn new(
        entry_point: &'arena RefCell<Function<'arena>>,
        functions: Vec<&'arena RefCell<Function<'arena>>>,
    ) -> Program<'arena> {
        Program {
            entry_point,
            functions,
        }
    }

    pub(super) fn entry_point(&self) -> &'arena RefCell<Function<'arena>> {
        self.entry_point
    }

    pub(super) fn functions(&self) -> &Vec<&'arena RefCell<Function<'arena>>> {
        &self.functions
    }
}

#[derive(Debug, Clone, Copy)]
enum FunctionKind {
    Eridani,
    Rust,
}

#[derive(Debug, Clone)]
pub(crate) enum Function<'arena> {
    Eridani {
        name: String,
        methods: Vec<&'arena RefCell<Method<'arena>>>,
    },
    Rust {
        name: String,
        func: EridaniFunction,
    },
}

impl<'arena> PartialEq for Function<'arena> {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (
                Function::Eridani {
                    name: _,
                    methods: methods1,
                },
                Function::Eridani {
                    name: _,
                    methods: methods2,
                },
            ) => methods1.as_ptr() == methods2.as_ptr(),
            (
                Function::Rust {
                    name: _,
                    func: func1,
                },
                Function::Rust {
                    name: _,
                    func: func2,
                },
            ) => func1 == func2,

            _ => false,
        }
    }
}

impl<'arena> Eq for Function<'arena> {}

impl<'arena> PartialOrd for Function<'arena> {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

impl<'arena> Ord for Function<'arena> {
    fn cmp(&self, other: &Self) -> Ordering {
        match (self, other) {
            (
                Function::Eridani {
                    methods: methods1, ..
                },
                Function::Eridani {
                    methods: methods2, ..
                },
            ) => methods1.as_ptr().cmp(&methods2.as_ptr()),
            (Function::Rust { func: func1, .. }, Function::Rust { func: func2, .. }) => {
                func1.cmp(func2)
            }
            (Function::Eridani { .. }, Function::Rust { .. }) => Ordering::Greater,
            (Function::Rust { .. }, Function::Eridani { .. }) => Ordering::Less,
        }
    }
}

impl<'arena> Function<'arena> {
    pub(crate) fn name(&self) -> &String {
        match self {
            Function::Eridani { name, .. } => name,
            Function::Rust { name, .. } => name,
        }
    }

    pub(super) fn resolve_placeholders(&self) -> Result<()> {
        if let Function::Eridani { methods, .. } = self {
            for method in methods {
                method.borrow_mut().body.resolve_placeholders()?;
            }
        }

        Ok(())
    }

    pub(super) fn methods(&self) -> Option<&Vec<&'arena RefCell<Method<'arena>>>> {
        if let Function::Eridani { methods, .. } = self {
            Some(methods)
        } else {
            None
        }
    }

    fn calls(&self) -> Vec<&'arena RefCell<Function<'arena>>> {
        if let Function::Eridani { methods, .. } = self {
            let mut calls = vec![];
            for method in methods {
                calls.append(&mut method.borrow().body.calls());
            }
            calls
        } else {
            vec![]
        }
    }

    pub(crate) fn native(&self) -> Option<EridaniFunction> {
        if let Function::Rust { func, .. } = self {
            Some(*func)
        } else {
            None
        }
    }

    fn kind(&self) -> FunctionKind {
        match self {
            Function::Eridani { .. } => FunctionKind::Eridani,
            Function::Rust { .. } => FunctionKind::Rust,
        }
    }

    fn destroy(&mut self) {
        if let Function::Eridani { methods, .. } = self {
            *methods = vec![];
        }
    }

    fn is_native(&self) -> bool {
        matches!(self, Function::Rust { .. })
    }
}

#[derive(Debug, Clone, PartialEq)]
pub(crate) struct Method<'arena> {
    args: Vec<(Option<(u16, bool)>, Pattern)>,
    arg_order: Vec<usize>,
    body: Expr<'arena>,
    environment: Box<Environment>,
    precedence: i32,
}

impl<'arena> Method<'arena> {
    pub(super) fn args(&self) -> &Vec<(Option<(u16, bool)>, Pattern)> {
        &self.args
    }

    pub(super) fn body(&self) -> &Expr<'arena> {
        &self.body
    }

    pub(super) fn arg_order(&self) -> &Vec<usize> {
        &self.arg_order
    }
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub(crate) enum BinOp {
    Add,
    Sub,
    Mul,
    Div,
    Mod,
}

impl BinOp {
    fn operate(self, left: &Value, right: &Value) -> Option<Value> {
        match self {
            BinOp::Add => left + right,
            BinOp::Sub => left - right,
            BinOp::Mul => left * right,
            BinOp::Div => left / right,
            BinOp::Mod => left % right,
        }
    }
}

impl fmt::Display for BinOp {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let string = match self {
            BinOp::Add => "+",
            BinOp::Sub => "-",
            BinOp::Mul => "*",
            BinOp::Div => "/",
            BinOp::Mod => "%",
        };

        write!(f, "{string}")
    }
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub(crate) enum UnOp {
    Negate,
}

impl UnOp {
    fn operate(self, value: &Value) -> Option<Value> {
        match self {
            UnOp::Negate => -value,
        }
    }
}

impl fmt::Display for UnOp {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let string = match self {
            UnOp::Negate => "!",
        };

        write!(f, "{string}")
    }
}

#[derive(Debug, Clone, PartialEq)]
pub(super) struct Environment {
    variables: BiMap<String, u16>,
    enclosing: Option<NonNull<Environment>>,
}

impl Environment {
    /// # Safety
    /// The enclosing environment must remain in the same place in memory
    /// while this environment exists.
    pub unsafe fn new(enclosing: &Environment) -> Box<Environment> {
        Box::new(Self {
            variables: BiMap::new(),
            enclosing: Some(NonNull::from(enclosing)),
        })
    }

    pub fn new_toplevel() -> Box<Environment> {
        Box::new(Self {
            variables: BiMap::new(),
            enclosing: None,
        })
    }

    fn new_reference(&self) -> u16 {
        if let Some(reference) = self.variables.right_values().max() {
            *reference + 1
        } else if let Some(enclosing) = self.enclosing {
            // SAFETY: `self.enclosing` is guaranteed to be valid
            // and we never mutate the enclosing environment
            let enclosing = unsafe { enclosing.as_ref() };
            enclosing.new_reference()
        } else {
            0
        }
    }

    #[must_use]
    pub fn get_or_add(&mut self, name: String) -> (u16, bool) {
        if let Some(reference) = self.get(&name) {
            (reference, true)
        } else {
            let reference = self.new_reference();
            self.variables.insert(name, reference);
            (reference, false)
        }
    }

    pub fn get(&self, name: &str) -> Option<u16> {
        if let Some(reference) = self.variables.get_by_left(name) {
            Some(*reference)
        } else if let Some(enclosing) = self.enclosing {
            // SAFETY: `self.enclosing` is guaranteed to be valid
            // and we never mutate the enclosing environment
            let enclosing = unsafe { enclosing.as_ref() };
            enclosing.get(name)
        } else {
            None
        }
    }
}

#[derive(Clone, PartialEq)]
pub(super) struct PlaceHolderModule<'arena>(&'arena RefCell<Module<'arena>>);

impl<'arena> fmt::Debug for PlaceHolderModule<'arena> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        fmt::Debug::fmt(&self.0, f)
    }
}

#[derive(Debug, Clone, PartialEq)]
pub(super) struct ListExpr<'arena> {
    expr: Expr<'arena>,
    spread: bool,
}

impl<'arena> ListExpr<'arena> {
    pub(super) fn expr(&self) -> &Expr<'arena> {
        &self.expr
    }

    pub(super) fn spread(&self) -> bool {
        self.spread
    }
}

#[derive(Clone, PartialEq)]
pub(super) enum Expr<'arena> {
    Binary {
        left: Box<Self>,
        operator: BinOp,
        right: Box<Self>,
    },
    Block {
        body: Vec<Self>,
        line: usize,
        environment: Box<Environment>,
    },
    Call {
        callee: Box<Self>,
        arguments: Vec<ListExpr<'arena>>,
        line: usize,
    },
    Function {
        function: &'arena RefCell<Function<'arena>>,
        line: usize,
    },
    Grouping(Box<Self>),
    Let {
        pattern: Pattern,
        value: Box<Self>,
    },
    List {
        expressions: Vec<ListExpr<'arena>>,
        line: usize,
    },
    ListItem(Box<ListExpr<'arena>>),
    Literal {
        value: Value,
        line: usize,
    },
    Method(Box<Method<'arena>>),
    PlaceHolder(ImportTree, PlaceHolderModule<'arena>),
    Unary {
        operator: UnOp,
        right: Box<Self>,
    },
    Variable {
        reference: u16,
        line: usize,
    },
}

impl<'arena> Expr<'arena> {
    fn resolve_placeholders(&mut self) -> Result<()> {
        match self {
            Self::Binary { left, right, .. } => {
                left.resolve_placeholders()?;
                right.resolve_placeholders()?;
            }
            Self::Block { body, .. } => {
                for expr in body {
                    expr.resolve_placeholders()?;
                }
            }
            Self::Call {
                callee, arguments, ..
            } => {
                callee.resolve_placeholders()?;
                for expr in arguments {
                    expr.expr.resolve_placeholders()?;
                }
            }
            Self::Function { .. } => {}
            Self::Grouping(expr) => expr.resolve_placeholders()?,
            Self::Let { value, .. } => value.resolve_placeholders()?,
            Self::List { expressions, .. } => {
                for expr in expressions {
                    expr.expr.resolve_placeholders()?;
                }
            }
            Self::ListItem(item) => item.expr.resolve_placeholders()?,
            Self::Literal { .. } => {}
            Self::Method(method) => method.body.resolve_placeholders()?,
            Self::PlaceHolder(placeholder, containing_module) => {
                let containing_module = containing_module.0;
                let function = if let Some(function) = containing_module
                    .borrow()
                    .find_function(placeholder.name().lexeme())
                {
                    function
                } else if let Some(binding) = containing_module
                    .borrow()
                    .bindings()
                    .get(placeholder.name().lexeme())
                {
                    if let Binding::Function(f) = binding {
                        *f
                    } else {
                        internal_error!("placeholder pointing to module")
                    }
                } else {
                    internal_error!(
                        "dangling placeholder {:?} in module '{}'",
                        placeholder,
                        containing_module.borrow().name()
                    )
                };

                *self = Self::Function {
                    function,
                    line: placeholder.line(),
                };
            }
            Self::Unary { right, .. } => right.resolve_placeholders()?,
            Self::Variable { .. } => {}
        }

        Ok(())
    }

    fn calls(&self) -> Vec<&'arena RefCell<Function<'arena>>> {
        match self {
            Self::Binary { left, right, .. } => {
                let mut calls = left.calls();
                calls.append(&mut right.calls());
                calls
            }
            Self::Block { body, .. } => {
                let mut calls = vec![];
                for expr in body {
                    calls.append(&mut expr.calls());
                }

                calls
            }
            Self::Call {
                callee, arguments, ..
            } => {
                let mut calls = callee.calls();

                for expr in arguments {
                    calls.append(&mut expr.expr.calls());
                }

                calls
            }
            Self::Function { function, .. } => vec![*function],
            Self::Grouping(expr) => expr.calls(),
            Self::Let { value, .. } => value.calls(),
            Self::List { expressions, .. } => expressions
                .iter()
                .flat_map(|expr| expr.expr.calls())
                .collect(),
            Self::ListItem(expr) => expr.expr.calls(),
            Self::Literal { .. } => vec![],
            Self::Method(method) => method.body.calls(),
            Self::PlaceHolder(_, _) => internal_error!("called 'calls' on placeholder"),
            Self::Unary { right, .. } => right.calls(),
            Self::Variable { .. } => vec![],
        }
    }

    pub(crate) fn line(&self) -> usize {
        match self {
            Expr::Binary { right, .. } => right.line(),
            Expr::Block { body, line, .. } => {
                if let Some(expr) = body.last() {
                    expr.line()
                } else {
                    *line
                }
            }
            Expr::Call { line, .. } => *line,
            Expr::Function { line, .. } => *line,
            Expr::Grouping(expr) => expr.line(),
            Expr::Let { value, .. } => value.line(),
            Expr::List { expressions, line } => {
                if let Some(expr) = expressions.last() {
                    expr.expr.line()
                } else {
                    *line
                }
            }
            Expr::ListItem(item) => item.expr.line(),
            Expr::Literal { line, .. } => *line,
            Expr::Method(method) => method.body.line(),
            Expr::PlaceHolder(placeholder, _) => placeholder.line(),
            Expr::Unary { right, .. } => right.line(),
            Expr::Variable { line, .. } => *line,
        }
    }
}

impl<'arena> fmt::Debug for Expr<'arena> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Expr::Binary {
                left,
                operator,
                right,
            } => f
                .debug_struct("Expr::Binary")
                .field("left", left)
                .field("operator", operator)
                .field("right", right)
                .finish(),
            Expr::Block {
                body,
                line,
                environment,
            } => f
                .debug_struct("Expr::Block")
                .field("body", body)
                .field("line", line)
                .field("environment", environment)
                .finish(),
            Expr::Call {
                callee,
                arguments,
                line,
            } => f
                .debug_struct("Expr::Call")
                .field("callee", callee)
                .field("arguments", arguments)
                .field("line", line)
                .finish(),
            Expr::Function { function, line } => f
                .debug_struct("Expr::Function")
                .field("function", function.borrow().name())
                .field("line", line)
                .field("kind", &function.borrow().kind())
                .finish(),
            Expr::Grouping(expr) => f.debug_tuple("Expr::Grouping").field(expr).finish(),
            Expr::Let { pattern, value } => f
                .debug_struct("Expr::Let")
                .field("pattern", pattern)
                .field("value", value)
                .finish(),
            Expr::List { expressions, line } => f
                .debug_struct("Expr::List")
                .field("expressions", expressions)
                .field("line", line)
                .finish(),
            Expr::ListItem(item) => f.debug_tuple("Expr::ListItem").field(item).finish(),
            Expr::Literal { value, line } => f
                .debug_struct("Expr::Literal")
                .field("value", value)
                .field("line", line)
                .finish(),
            Expr::Method(method) => f.debug_tuple("Expr::Method").field(method).finish(),
            Expr::PlaceHolder(placeholder, module) => f
                .debug_tuple("Expr::Placeholder")
                .field(placeholder)
                .field(module)
                .finish(),
            Expr::Unary { operator, right } => f
                .debug_struct("Expr::Unary")
                .field("operator", operator)
                .field("right", right)
                .finish(),
            Expr::Variable { reference, line } => f
                .debug_struct("Expr::Variable")
                .field("reference", reference)
                .field("line", line)
                .finish(),
        }
    }
}

#[derive(Debug, Clone)]
enum Binding<'arena> {
    Function(&'arena RefCell<Function<'arena>>),
    Module(&'arena RefCell<Module<'arena>>),
    PlaceHolder(String, &'arena RefCell<Module<'arena>>),
}

impl<'arena> Binding<'arena> {
    fn name(&self) -> String {
        match self {
            Binding::Function(f) => f.borrow().name().to_string(),
            Binding::Module(m) => m.borrow().name().clone(),
            Binding::PlaceHolder(name, _) => name.clone(),
        }
    }
}

fn clone_bindings<'arena>(
    bindings: &BTreeMap<String, Binding<'arena>>,
) -> BTreeMap<String, Binding<'arena>> {
    BTreeMap::from_iter(bindings.iter().map(|(a, b)| (a.clone(), b.clone())))
}

#[cfg(test)]
fn analyse_unoptimised<'arena>(
    arena: &'arena Arena,
    parse_tree: ParseTree,
    source_origin: Option<String>,
    entry_point: &str,
) -> Result<Program<'arena>> {
    let converted = converter::convert(arena, parse_tree, source_origin, entry_point)?;
    let checked = checker::check_before_optimisation(converted)?;
    Ok(checked)
}

#[cfg(test)]
fn optimise(program: Program) -> Result<Program> {
    optimiser::optimise(program)
}

pub(super) fn analyse<'arena>(
    arena: &'arena Arena,
    parse_tree: ParseTree,
    source_origin: Option<String>,
    entry_point: &str,
) -> Result<Program<'arena>> {
    let converted = converter::convert(arena, parse_tree, source_origin, entry_point)?;
    let checked = checker::check_before_optimisation(converted)?;
    let optimised = optimiser::optimise(checked)?;
    Ok(optimised)
}

#[cfg(test)]
mod tests;

#[cfg(feature = "tree_walk")]
mod treewalk;
