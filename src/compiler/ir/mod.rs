mod checker;
mod converter;
mod match_engine;
mod modules;
mod optimiser;
pub(super) mod pattern;

use alloc::rc::Rc;
use modules::Module;

use crate::{
    common::{internal_error, value::Value, EridaniFunction},
    compiler::{
        ir::pattern::{Item, LogOp, Pattern, Variable},
        parser::{ImportTree, ParseTree},
        Result,
    },
    prelude::*,
};

use core::{cell::RefCell, cmp::Ordering, fmt};

use bimap::BiMap;

use hashbrown::HashMap;

pub(crate) struct Program {
    entry_point: Rc<RefCell<Function>>,
    functions: Vec<Rc<RefCell<Function>>>,
    source: String,
}

impl Program {
    fn new(
        entry_point: Rc<RefCell<Function>>,
        functions: Vec<Rc<RefCell<Function>>>,
        source: String,
    ) -> Program {
        Program {
            entry_point,
            functions,
            source,
        }
    }

    pub(super) const fn entry_point(&self) -> &Rc<RefCell<Function>> {
        &self.entry_point
    }

    pub(super) fn functions(&self) -> &[Rc<RefCell<Function>>] {
        &self.functions
    }

    pub(super) fn source(&self) -> &str {
        &self.source
    }
}

impl Drop for Program {
    fn drop(&mut self) {
        *self.entry_point.borrow_mut() = Function::Eridani {
            name: String::new(),
            methods: vec![],
        };
        for function in &self.functions {
            *function.borrow_mut() = Function::Eridani {
                name: String::new(),
                methods: vec![],
            };
        }
    }
}

#[derive(Debug, Clone, Copy)]
enum FunctionKind {
    Eridani,
    Rust,
}

#[derive(Debug, Clone)]
pub(crate) enum Function {
    Eridani {
        name: String,
        methods: Vec<Rc<RefCell<Method>>>,
    },
    Rust {
        name: String,
        func: EridaniFunction,
    },
}

impl PartialEq for Function {
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

impl Eq for Function {}

impl PartialOrd for Function {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for Function {
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

impl Function {
    pub(crate) fn name(&self) -> &str {
        match self {
            Function::Eridani { name, .. } | Function::Rust { name, .. } => name,
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

    pub(super) fn methods(&self) -> Option<&[Rc<RefCell<Method>>]> {
        if let Function::Eridani { methods, .. } = self {
            Some(methods)
        } else {
            None
        }
    }

    fn calls(&self) -> Vec<Rc<RefCell<Function>>> {
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

    const fn kind(&self) -> FunctionKind {
        match self {
            Function::Eridani { .. } => FunctionKind::Eridani,
            Function::Rust { .. } => FunctionKind::Rust,
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub(crate) struct Method {
    args: Vec<(Option<(u16, bool)>, Pattern)>,
    arg_order: Vec<usize>,
    body: Expr,
    environment: Rc<RefCell<Environment>>,
    precedence: i32,
}

impl Method {
    pub(super) fn args(&self) -> &[(Option<(u16, bool)>, Pattern)] {
        &self.args
    }

    pub(super) const fn body(&self) -> &Expr {
        &self.body
    }

    pub(super) fn arg_order(&self) -> &[usize] {
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
    enclosing: Option<Rc<RefCell<Environment>>>,
}

impl Environment {
    pub fn new(enclosing: Rc<RefCell<Environment>>) -> Environment {
        Self {
            variables: BiMap::new(),
            enclosing: Some(enclosing),
        }
    }

    pub fn new_toplevel() -> Environment {
        Self {
            variables: BiMap::new(),
            enclosing: None,
        }
    }

    fn new_reference(&self) -> u16 {
        self.variables.right_values().max().map_or_else(
            || {
                self.enclosing
                    .as_deref()
                    .map_or(0, |enclosing| enclosing.borrow().new_reference())
            },
            |reference| *reference + 1,
        )
    }

    #[must_use]
    pub fn get_or_add(&mut self, name: String) -> (u16, bool) {
        self.get(&name).map_or_else(
            || {
                let reference = self.new_reference();
                self.variables.insert(name, reference);
                (reference, false)
            },
            |reference| (reference, true),
        )
    }

    pub fn get(&self, name: &str) -> Option<u16> {
        self.variables.get_by_left(name).map_or_else(
            || {
                self.enclosing
                    .as_deref()
                    .and_then(|enclosing| enclosing.borrow().get(name))
            },
            |reference| Some(*reference),
        )
    }
}

#[derive(Clone, PartialEq)]
pub(super) struct PlaceHolderModule(Rc<RefCell<Module>>);

impl fmt::Debug for PlaceHolderModule {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        fmt::Debug::fmt(&self.0, f)
    }
}

#[derive(Debug, Clone, PartialEq)]
pub(super) struct ListExpr {
    expr: Expr,
    spread: bool,
}

impl ListExpr {
    pub(super) const fn expr(&self) -> &Expr {
        &self.expr
    }

    pub(super) const fn spread(&self) -> bool {
        self.spread
    }
}

#[derive(Clone, PartialEq)]
pub(super) enum Expr {
    Binary {
        left: Box<Self>,
        operator: BinOp,
        right: Box<Self>,
    },
    Block {
        body: Vec<Self>,
        line: usize,
        environment: Rc<RefCell<Environment>>,
    },
    Call {
        callee: Box<Self>,
        arguments: Vec<ListExpr>,
        line: usize,
    },
    Function {
        function: Rc<RefCell<Function>>,
        line: usize,
    },
    Grouping(Box<Self>),
    Let {
        pattern: Pattern,
        value: Box<Self>,
    },
    List {
        expressions: Vec<ListExpr>,
        line: usize,
    },
    ListItem(Box<ListExpr>),
    Literal {
        value: Value,
        line: usize,
    },
    Method(Box<Method>),
    PlaceHolder(ImportTree, PlaceHolderModule),
    Unary {
        operator: UnOp,
        right: Box<Self>,
    },
    Variable {
        reference: u16,
        line: usize,
    },
}

impl Expr {
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
            Self::Grouping(expr) => expr.resolve_placeholders()?,
            Self::Let { value, .. } => value.resolve_placeholders()?,
            Self::List { expressions, .. } => {
                for expr in expressions {
                    expr.expr.resolve_placeholders()?;
                }
            }
            Self::ListItem(item) => item.expr.resolve_placeholders()?,
            Self::Method(method) => method.body.resolve_placeholders()?,
            Self::PlaceHolder(placeholder, containing_module) => {
                let containing_module = Rc::clone(&containing_module.0);
                let function = containing_module
                    .borrow()
                    .find_function(placeholder.name().lexeme())
                    .map_or_else(
                        || {
                            if let Some(binding) = containing_module
                                .borrow()
                                .bindings()
                                .get(placeholder.name().lexeme())
                            {
                                if let Binding::Function(f) = binding {
                                    Rc::clone(f)
                                } else {
                                    internal_error!("placeholder pointing to module")
                                }
                            } else {
                                internal_error!(
                                    "dangling placeholder {:?} in module '{}'",
                                    placeholder,
                                    containing_module.borrow().name()
                                )
                            }
                        },
                        |function| function,
                    );

                *self = Self::Function {
                    function,
                    line: placeholder.line(),
                };
            }
            Self::Unary { right, .. } => right.resolve_placeholders()?,
            Self::Function { .. } | Self::Literal { .. } | Self::Variable { .. } => {}
        }

        Ok(())
    }

    fn calls(&self) -> Vec<Rc<RefCell<Function>>> {
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
            Self::Function { function, .. } => vec![Rc::clone(function)],
            Self::Grouping(expr) => expr.calls(),
            Self::Let { value, .. } => value.calls(),
            Self::List { expressions, .. } => expressions
                .iter()
                .flat_map(|expr| expr.expr.calls())
                .collect(),
            Self::ListItem(expr) => expr.expr.calls(),
            Self::Literal { .. } | Self::Variable { .. } => vec![],
            Self::Method(method) => method.body.calls(),
            Self::PlaceHolder(_, _) => internal_error!("called 'calls' on placeholder"),
            Self::Unary { right, .. } => right.calls(),
        }
    }

    pub(crate) fn line(&self) -> usize {
        match self {
            Expr::Binary { right, .. } | Expr::Unary { right, .. } => right.line(),
            Expr::Block { body, line, .. } => body.last().map_or(*line, Expr::line),
            Expr::Call { line, .. }
            | Expr::Function { line, .. }
            | Expr::Literal { line, .. }
            | Expr::Variable { line, .. } => *line,
            Expr::Grouping(expr) => expr.line(),
            Expr::Let { value, .. } => value.line(),
            Expr::List { expressions, line } => {
                expressions.last().map_or(*line, |expr| expr.expr.line())
            }
            Expr::ListItem(item) => item.expr.line(),
            Expr::Method(method) => method.body.line(),
            Expr::PlaceHolder(placeholder, _) => placeholder.line(),
        }
    }
}

impl fmt::Debug for Expr {
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
                .field("function", &function.borrow().name())
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
enum Binding {
    Function(Rc<RefCell<Function>>),
    Module(Rc<RefCell<Module>>),
    PlaceHolder(String, Rc<RefCell<Module>>),
}

impl Binding {
    fn name(&self) -> String {
        match self {
            Binding::Function(f) => f.borrow().name().to_string(),
            Binding::Module(m) => m.borrow().name().to_string(),
            Binding::PlaceHolder(name, _) => name.clone(),
        }
    }
}

fn clone_bindings(bindings: &HashMap<String, Binding>) -> HashMap<String, Binding> {
    bindings
        .iter()
        .map(|(a, b)| (a.clone(), b.clone()))
        .collect()
}

pub(super) fn analyse(
    parse_tree: &ParseTree,
    source_origin: Option<String>,
    entry_point: &str,
) -> Result<Program> {
    let converted = converter::convert(parse_tree, source_origin, entry_point)?;
    let checked = checker::check_before_optimisation(converted)?;
    let optimised = optimiser::optimise(checked)?;
    Ok(optimised)
}
