mod match_engine;
mod modules;
mod pattern;

use match_engine::resolve_metapatterns;
pub(crate) use match_engine::{match_args, MatchResult};
use modules::{get_library, resolve_import, Module, Visibility};

use crate::{
    common::{internal_error, natives, value::Value, EridaniFunction},
    compiler::{
        analyser::pattern::{Item, LogOp, Pattern, Variable},
        parser::{self, ImportTree, ParseTree},
        scanner::{self, TokenType},
        Error, Result,
    },
};

use alloc::{collections::BTreeMap, rc::Rc};
use core::{cell::RefCell, fmt, ptr::NonNull};

use bimap::BiMap;

pub struct Program {
    entry_point: Rc<RefCell<Function>>,
    functions: Vec<Rc<RefCell<Function>>>,
    #[cfg(feature = "std")]
    libraries: Vec<NonNull<Library>>,
}

impl Program {
    fn new(entry_point: Rc<RefCell<Function>>, functions: Vec<Rc<RefCell<Function>>>) -> Program {
        Program {
            entry_point,
            functions,
            #[cfg(feature = "std")]
            libraries: vec![],
        }
    }

    pub(crate) fn entry_point(&self) -> &Rc<RefCell<Function>> {
        &self.entry_point
    }

    pub(crate) fn functions(&self) -> &Vec<Rc<RefCell<Function>>> {
        &self.functions
    }

    #[cfg(feature = "std")]
    pub(crate) fn libraries(&mut self) -> Vec<NonNull<Library>> {
        let libraries = self.libraries.clone();
        self.libraries = vec![];
        libraries
    }

    #[cfg(feature = "std")]
    fn push_library(&mut self, library: NonNull<Library>) {
        self.libraries.push(library);
    }

    #[cfg(feature = "no_std")]
    #[inline(always)]
    fn push_library<T>(&self, _library: T) {}
}

impl Drop for Program {
    fn drop(&mut self) {
        for function in &self.functions {
            if let Ok(mut function) = function.try_borrow_mut() {
                function.destroy();
            }
        }

        #[cfg(feature = "std")]
        for &library in &self.libraries {
            // SAFETY: this is the only pointer to this value
            unsafe { Box::from_raw(library.as_ptr()) };
        }
    }
}

#[cfg(feature = "std")]
type Library = libloading::Library;

#[cfg(not(feature = "std"))]
type Library = ();

#[derive(Debug, Clone, Copy)]
enum FunctionKind {
    Eridani,
    Rust,
}

pub enum Function {
    Eridani {
        name: String,
        methods: Vec<RefCell<Method>>,
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

impl fmt::Debug for Function {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Function::Rust { name, .. } => f
                .debug_struct("Function::Rust")
                .field("name", name)
                .finish(),
            Function::Eridani { name, methods } => f
                .debug_struct("Function::Eridani")
                .field("name", name)
                .field("methods", methods)
                .finish(),
        }
    }
}

impl PartialEq for Function {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (
                Function::Eridani {
                    name: name1,
                    methods: methods1,
                },
                Function::Eridani {
                    name: name2,
                    methods: methods2,
                },
            ) => {
                if name1 != name2 {
                    return false;
                }

                for (method1, method2) in methods1.iter().zip(methods2) {
                    if method1.as_ptr() != method2.as_ptr() {
                        return false;
                    }
                }

                true
            }
            (
                Function::Rust {
                    name: _,
                    func: func1,
                },
                Function::Rust {
                    name: _,
                    func: func2,
                },
            ) => {
                let ptr1: *const Box<dyn EridaniFunction> = func1;
                let ptr2: *const Box<dyn EridaniFunction> = func2;

                ptr1 == ptr2
            }
            _ => false,
        }
    }
}

impl Function {
    pub fn name(&self) -> &String {
        match self {
            Function::Eridani { name, .. } => name,
            Function::Rust { name, .. } => name,
        }
    }

    fn resolve_placeholders(&self) -> Result<()> {
        if let Function::Eridani { methods, .. } = self {
            for method in methods {
                method.borrow_mut().body.resolve_placeholders()?;
            }
        }

        Ok(())
    }

    pub fn methods(&self) -> &Vec<RefCell<Method>> {
        if let Function::Eridani { methods, .. } = self {
            methods
        } else {
            internal_error!("attempted to get methods of native")
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

    pub fn native(&self) -> Option<Box<dyn EridaniFunction>> {
        if let Function::Rust { func, .. } = self {
            Some(func.to_owned())
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
}

#[derive(Debug, Clone, PartialEq)]
pub struct Method {
    args: Vec<(Option<u16>, Pattern)>,
    arg_order: Vec<usize>,
    body: Expr,
    environment: Box<Environment>,
}

impl Method {
    pub(crate) fn args(&self) -> &Vec<(Option<u16>, Pattern)> {
        &self.args
    }

    pub(crate) fn body(&self) -> &Expr {
        &self.body
    }

    pub(crate) fn precedence(&self, bindings: &[u16]) -> i32 {
        self.args
            .iter()
            .map(|(_, pattern)| pattern)
            .fold(0, |a, b| a + b.precedence(bindings))
    }

    pub(crate) fn arg_order(&self) -> &Vec<usize> {
        &self.arg_order
    }
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum BinOp {
    Add,
    Sub,
    Mul,
    Div,
    Mod,
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
pub enum UnOp {
    Negate,
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
pub struct Environment {
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
    pub fn get_or_add(&mut self, name: String) -> u16 {
        if let Some(reference) = self.get(&name) {
            reference
        } else {
            let reference = self.new_reference();
            self.variables.insert(name, reference);
            reference
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
pub(crate) struct PlaceHolderModule(Rc<RefCell<Module>>);

impl fmt::Debug for PlaceHolderModule {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        fmt::Debug::fmt(&self.0, f)
    }
}

#[derive(Debug, Clone, PartialEq)]
pub(crate) struct ListExpr {
    expr: Expr,
    spread: bool,
}

impl ListExpr {
    pub(crate) fn expr(self) -> Expr {
        self.expr
    }

    pub(crate) fn spread(&self) -> bool {
        self.spread
    }
}

#[derive(Clone, PartialEq)]
pub(crate) enum Expr {
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
                let containing_module = &containing_module.0;
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
                        f.clone()
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

struct Calls {
    calls: Vec<Rc<RefCell<Function>>>,
}

impl Calls {
    fn new() -> Self {
        Calls { calls: vec![] }
    }

    fn push(&mut self, function: Rc<RefCell<Function>>) {
        if !self.calls.contains(&function) {
            self.calls.push(Rc::clone(&function));
            for call in function.borrow().calls() {
                self.push(call);
            }
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
            Binding::Module(m) => m.borrow().name().clone(),
            Binding::PlaceHolder(name, _) => name.clone(),
        }
    }
}

#[allow(clippy::type_complexity)]
fn get_std() -> Result<(Vec<Rc<RefCell<Module>>>, BTreeMap<String, Binding>)> {
    let mut eridani_std = super::eridani_std::ERIDANI_STD_BASIC.to_owned();

    #[cfg(feature = "target_std")]
    (eridani_std += super::eridani_std::ERIDANI_STD_FEATURE_STD);

    #[cfg(feature = "target_web")]
    (eridani_std += super::eridani_std::ERIDANI_STD_FEATURE_WEB);

    let std_module = Rc::new(RefCell::new(Module::new(
        "std",
        BTreeMap::new(),
        None,
        None,
    )));

    let natives_module = Rc::new(RefCell::new(Module::new(
        "internals",
        BTreeMap::new(),
        None,
        Some(&std_module),
    )));
    for (name, func) in natives::NATIVES {
        natives_module
            .borrow_mut()
            .push_function(Rc::new(RefCell::new(Function::Rust {
                name: name.to_string(),
                func: Box::new(func),
            })));
    }

    std_module
        .borrow_mut()
        .push_submodule(Rc::clone(&natives_module), Visibility::Private);

    let mut modules = vec![Rc::clone(&std_module), Rc::clone(&natives_module)];

    let eridani_std = parser::parse(scanner::scan(eridani_std)?)?;
    analyse_module(eridani_std, &std_module, &mut modules)?;

    let prelude = super::eridani_std::PRELUDE;
    let mut bindings = BTreeMap::new();
    bindings.insert("std".to_string(), Binding::Module(Rc::clone(&std_module)));
    for function in std_module.borrow().functions() {
        if prelude.contains(&function.borrow().name().as_str()) {
            bindings.insert(
                function.borrow().name().to_string(),
                Binding::Function(Rc::clone(function)),
            );
        }
    }

    Ok((modules, bindings))
}

fn clone_bindings(bindings: &BTreeMap<String, Binding>) -> BTreeMap<String, Binding> {
    BTreeMap::from_iter(bindings.iter().map(|(a, b)| (a.clone(), b.clone())))
}

fn slices_overlap<T: PartialEq>(s1: &[T], s2: &[T]) -> bool {
    if s1.len() != s2.len() {
        return false;
    }

    for item in s1 {
        if !s2.contains(item) {
            return false;
        }
    }
    for item in s2 {
        if !s1.contains(item) {
            return false;
        }
    }

    true
}

fn verify_pattern(pattern: &Pattern, line: usize, resolved_names: &[u16]) -> Result<()> {
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
                    if !resolved_names.contains(&var.reference()) {
                        if wildcard {
                            return Err(Error::new(
                                line,
                                "Pattern",
                                "",
                                "Sequential wildcards in concatenations are not allowed",
                            ));
                        } else {
                            wildcard = true;
                        }
                    } else {
                        wildcard = false;
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
        Pattern::Range {
            lower,
            upper,
            inclusive,
        } => {
            let lower = lower.expect_number();
            let upper = upper.expect_number();
            if lower > upper || !inclusive && lower == upper {
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

pub fn analyse(
    parse_tree: ParseTree,
    source_origin: Option<String>,
    entry_point: &str,
) -> Result<Program> {
    let (mut modules, default_bindings) = get_std()?;
    let root_module = Rc::new(RefCell::new(Module::new(
        "",
        clone_bindings(&default_bindings),
        source_origin,
        None,
    )));
    modules.push(Rc::clone(&root_module));
    analyse_module(parse_tree, &root_module, &mut modules)?;

    let entry_point = match root_module.borrow().find_function(entry_point) {
        Some(entry_point) => entry_point,
        None => {
            let message = format!("Cannot find entry point '{entry_point}'");
            return Err(Error::new(1, "Name", "", &message));
        }
    };

    let mut all_functions: Vec<Rc<RefCell<Function>>> = vec![];

    for module in &modules {
        module.borrow().resolve_placeholders()?;
        for function in module.borrow().functions() {
            if !all_functions
                .iter()
                .any(|f| f.as_ptr() as usize == function.as_ptr() as usize)
            {
                all_functions.push(Rc::clone(function));
            }
        }
    }

    let mut calls = Calls::new();
    calls.push(Rc::clone(&entry_point));
    let functions = calls.calls;

    for function in all_functions {
        if !functions.contains(&function) {
            function.borrow_mut().destroy();
        }
    }

    let mut program = Program::new(entry_point, functions);

    for module in modules {
        if let Some(library) = get_library(module.borrow_mut()) {
            program.push_library(library);
        }
        module.borrow_mut().destroy();
    }

    Ok(program)
}

fn analyse_module(
    parse_tree: ParseTree,
    module: &Rc<RefCell<Module>>,
    modules: &mut Vec<Rc<RefCell<Module>>>,
) -> Result<()> {
    module.borrow_mut().init_names(&parse_tree);

    for (public, module_name) in parse_tree.modules() {
        let visibility = if public.is_some() {
            Visibility::Public
        } else {
            Visibility::Private
        };
        Module::resolve_submodule(
            module,
            module_name.lexeme(),
            module_name.line(),
            modules,
            visibility,
        )?;
    }

    for import in parse_tree.imports() {
        let (imported_module, in_module_tree) =
            module.borrow().find_imported_module(import.name())?;
        modules.push(Rc::clone(&imported_module));

        let binding = resolve_import(import.clone(), Rc::clone(&imported_module), in_module_tree)?;

        module.borrow_mut().add_binding(binding.name(), binding);
    }

    for (i, function) in parse_tree.functions().iter().enumerate() {
        let name = module.borrow().function_name(i).clone();

        let mut methods = vec![];
        for method in function.methods() {
            let method = analyse_method(method, None, module, modules)?;

            methods.push(RefCell::new(method));
        }

        module
            .borrow_mut()
            .push_function(Rc::new(RefCell::new(Function::Eridani { name, methods })));
    }

    Ok(())
}

#[allow(clippy::borrowed_box)] // This is required for safety
fn analyse_method(
    method: &parser::Method,
    enclosing: Option<&Box<Environment>>,
    module: &Rc<RefCell<Module>>,
    modules: &mut Vec<Rc<RefCell<Module>>>,
) -> Result<Method> {
    let mut environment = if let Some(enclosing) = enclosing {
        // SAFETY: `enclosing` is boxed and will never move
        unsafe { Environment::new(enclosing) }
    } else {
        Environment::new_toplevel()
    };

    let mut lines = vec![];
    let mut args: Vec<(Option<&String>, Pattern)> = method
        .args()
        .iter()
        .map(|pattern| {
            lines.push(pattern.line());
            (pattern.name(), pattern.pattern().into())
        })
        .collect();

    let arg_order = if let Ok(order) = resolve_metapatterns(&args) {
        order
    } else {
        return Err(Error::new(
            lines[0],
            "Pattern",
            "",
            "Circular dependencies in method parameters",
        ));
    };

    let mut references = vec![None; args.len()];
    let mut ordered_references = vec![];
    for &index in &arg_order {
        let (name, pattern) = &mut args[index];

        // this order is important!
        pattern.bind(&mut environment);
        if let Some(name) = name {
            let reference = environment.get_or_add(name.to_owned());
            references[index] = Some(reference);
            ordered_references.push(Some(reference));
        } else {
            ordered_references.push(None);
        }
    }

    for &index in &arg_order {
        let (_, pattern) = &args[index];
        let resolved_names: Vec<u16> = ordered_references[..=index]
            .iter()
            .copied()
            .flatten()
            .collect();
        verify_pattern(pattern, lines[index], &resolved_names)?;
    }

    let args = args
        .into_iter()
        .zip(references)
        .map(|((_, pattern), reference)| (reference, pattern))
        .collect();

    let body = convert_expr(method.body(), Rc::clone(module), &mut environment, modules)?;

    Ok(Method {
        args,
        arg_order,
        body,
        environment,
    })
}

fn convert_expr(
    expr: &parser::Expr,
    module: Rc<RefCell<Module>>,
    environment: &mut Box<Environment>,
    modules: &mut Vec<Rc<RefCell<Module>>>,
) -> Result<Expr> {
    use parser::Expr as ParseExpr;

    let result = match expr {
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

            let left = convert_expr(left, Rc::clone(&module), environment, modules)?;
            let right = convert_expr(right, module, environment, modules)?;

            Expr::Binary {
                left: Box::new(left),
                operator,
                right: Box::new(right),
            }
        }
        ParseExpr::Block { body, end } => {
            // SAFETY: `environment` is boxed and will not move
            let mut inner_environment = unsafe { Environment::new(environment) };
            let body = convert_exprs(body, &module, &mut inner_environment, modules)?;

            Expr::Block {
                body,
                line: end.line(),
                environment: inner_environment,
            }
        }
        ParseExpr::Call {
            callee,
            paren,
            arguments,
        } => {
            let callee = convert_expr(callee, Rc::clone(&module), environment, modules)?;

            let arguments = convert_list(arguments, &module, environment, modules)?;

            Expr::Call {
                callee: Box::new(callee),
                arguments,
                line: paren.line(),
            }
        }
        ParseExpr::Grouping(expr) => {
            let expr = convert_expr(expr, Rc::clone(&module), environment, modules)?;
            Expr::Grouping(Box::new(expr))
        }
        ParseExpr::Let { pattern, value } => {
            let value = convert_expr(value, module, environment, modules)?;
            let mut pattern: Pattern = pattern.into();
            pattern.bind(environment);

            Expr::Let {
                pattern,
                value: Box::new(value),
            }
        }
        ParseExpr::List { expressions, end } => Expr::List {
            expressions: convert_list(expressions, &module, environment, modules)?,
            line: end.line(),
        },
        ParseExpr::Literal(token) => {
            let line = token.line();

            Expr::Literal {
                value: token.into(),
                line,
            }
        }
        ParseExpr::Method(method) => {
            let method = analyse_method(method, Some(environment), &module, modules)?;

            Expr::Method(Box::new(method))
        }
        ParseExpr::Unary { operator, right } => {
            let right = convert_expr(right, module, environment, modules)?;

            match operator.kind() {
                TokenType::DotDot => Expr::ListItem(Box::new(ListExpr {
                    expr: right,
                    spread: true,
                })),
                TokenType::Minus => Expr::Unary {
                    operator: UnOp::Negate,
                    right: Box::new(right),
                },
                _ => internal_error!("parsed '{:?}' as unary operator", operator),
            }
        }
        ParseExpr::Variable(var) => {
            if var.next().is_some() {
                let (import_module, looking_from_module_tree) =
                    module.borrow().find_imported_module(var.name())?;
                let item = resolve_import(var.clone(), import_module, looking_from_module_tree)?;

                match item {
                    Binding::Function(function) => Expr::Function {
                        function,
                        line: var.line(),
                    },
                    Binding::Module(module) => {
                        let location = format!(" at {}", module.borrow().name());
                        return Err(Error::new(
                            var.line(),
                            "Import",
                            &location,
                            "Expected function, found module",
                        ));
                    }
                    Binding::PlaceHolder(_, module) => {
                        Expr::PlaceHolder(var.last().to_owned(), PlaceHolderModule(module))
                    }
                }
            } else if let Some(reference) = environment.get(var.name().lexeme()) {
                Expr::Variable {
                    reference,
                    line: var.line(),
                }
            } else if module
                .borrow()
                .function_names()
                .contains(var.name().lexeme())
            {
                Expr::PlaceHolder(var.clone(), PlaceHolderModule(module))
            } else if let Some(binding) = module.borrow().bindings().get(var.name().lexeme()) {
                if let Binding::Function(function) = binding {
                    Expr::Function {
                        function: Rc::clone(function),
                        line: var.line(),
                    }
                } else {
                    let location = format!(" at {}", var.name().lexeme());
                    return Err(Error::new(
                        var.line(),
                        "Name",
                        &location,
                        "Cannot use module as variable",
                    ));
                }
            } else if let Some(reference) = environment.get(var.name().lexeme()) {
                Expr::Variable {
                    reference,
                    line: var.line(),
                }
            } else {
                let location = format!(" at {}", var.name().lexeme());
                let message = format!("Unknown name '{}'", var.name().lexeme());
                return Err(Error::new(var.line(), "Name", &location, &message));
            }
        }
    };

    Ok(result)
}

fn convert_exprs(
    values: &[parser::Expr],
    module: &Rc<RefCell<Module>>,
    environment: &mut Box<Environment>,
    modules: &mut Vec<Rc<RefCell<Module>>>,
) -> Result<Vec<Expr>> {
    let mut result = vec![];
    for value in values {
        let expr = convert_expr(value, Rc::clone(module), environment, modules)?;

        result.push(expr);
    }
    Ok(result)
}

fn convert_list(
    values: &[parser::Expr],
    module: &Rc<RefCell<Module>>,
    environment: &mut Box<Environment>,
    modules: &mut Vec<Rc<RefCell<Module>>>,
) -> Result<Vec<ListExpr>> {
    let mut result = vec![];
    for value in values {
        let expr = convert_expr(value, Rc::clone(module), environment, modules)?;
        let expr = if let Expr::ListItem(expression) = expr {
            *expression
        } else {
            ListExpr {
                expr,
                spread: false,
            }
        };
        result.push(expr);
    }
    Ok(result)
}
