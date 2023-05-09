use crate::{
    common::{
        internal_error, natives,
        value::{self, Pattern, Value},
        EridaniFunction,
    },
    compiler::{
        parser::{self, ImportTree, ParseTree},
        scanner::{self, TokenType},
        Error, Result,
    },
    prelude::*,
};

use alloc::{
    collections::BTreeMap,
    rc::{Rc, Weak},
};
use core::{
    cell::RefCell,
    fmt::{self, Debug},
};

#[cfg(not(feature = "no_std"))]
use std::{fs, path};

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
                    name: name1,
                    func: _,
                },
                Function::Rust {
                    name: name2,
                    func: _,
                },
            ) => {
                // TODO add proper comparison
                name1 == name2
            }
            _ => false,
        }
    }
}

impl Function {
    pub fn name(&self) -> &str {
        match self {
            Function::Eridani { name, .. } => name,
            Function::Rust { name, .. } => name,
        }
    }

    fn resolve_placeholders(&self, module: &Rc<RefCell<Module>>) -> Result<()> {
        if let Function::Eridani { methods, .. } = self {
            for method in methods {
                method.borrow_mut().body.resolve_placeholders(module)?;
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
            Some(func.clone())
        } else {
            None
        }
    }
}

pub struct Program(
    pub(crate) Rc<RefCell<Function>>,
    pub(crate) Vec<Rc<RefCell<Function>>>,
);

#[derive(Debug, Clone, PartialEq)]
pub struct Method {
    args: Vec<(Option<String>, Pattern)>,
    body: Expr,
}

impl Method {
    pub fn args(&self) -> &Vec<(Option<String>, Pattern)> {
        &self.args
    }

    pub fn body(&self) -> &Expr {
        &self.body
    }

    pub fn precedence(&self, bindings: &[Option<String>]) -> i32 {
        self.args
            .iter()
            .map(|(_, pattern)| pattern)
            .fold(0, |a, b| a + b.precedence(bindings))
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
        match self {
            BinOp::Add => write!(f, "+"),
            BinOp::Sub => write!(f, "-"),
            BinOp::Mul => write!(f, "*"),
            BinOp::Div => write!(f, "/"),
            BinOp::Mod => write!(f, "%"),
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum UnOp {
    Negate,
}

#[derive(Debug, Clone, PartialEq)]
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
        expressions: Vec<Self>,
        line: usize,
    },
    Literal {
        value: Value,
        line: usize,
    },
    Method(Box<Method>),
    PlaceHolder(ImportTree),
    Unary {
        operator: UnOp,
        right: Box<Self>,
    },
    Variable {
        name: String,
        line: usize,
    },
}

impl Expr {
    fn resolve_placeholders(&mut self, module: &Rc<RefCell<Module>>) -> Result<()> {
        match self {
            Self::Binary { left, right, .. } => {
                left.resolve_placeholders(module)?;
                right.resolve_placeholders(module)?;
            }
            Self::Block { body, .. } => {
                for expr in body {
                    expr.resolve_placeholders(module)?;
                }
            }
            Self::Call {
                callee, arguments, ..
            } => {
                callee.resolve_placeholders(module)?;
                for expr in arguments {
                    expr.resolve_placeholders(module)?;
                }
            }
            Self::Function { .. } => {}
            Self::Grouping(expr) => expr.resolve_placeholders(module)?,
            Self::Let { value, .. } => value.resolve_placeholders(module)?,
            Self::List { expressions, .. } => {
                for expr in expressions {
                    expr.resolve_placeholders(module)?;
                }
            }
            Self::Literal { .. } => {}
            Self::Method(method) => method.body.resolve_placeholders(module)?,
            Self::PlaceHolder(placeholder) => {
                if placeholder.next().is_some() {
                    unreachable!("currently no way to create a placeholder from an import tree");
                } else {
                    let function = if let Some(function) =
                        module.borrow().find_function(placeholder.name().lexeme())
                    {
                        function
                    } else if let Some(binding) =
                        module.borrow().bindings.get(placeholder.name().lexeme())
                    {
                        if let Binding::Function(f) = binding {
                            f.clone()
                        } else {
                            internal_error!("placeholder pointing to module")
                        }
                    } else {
                        internal_error!("dangling placeholder")
                    };

                    *self = Self::Function {
                        function,
                        line: placeholder.line(),
                    };
                }
            }
            Self::Unary { right, .. } => right.resolve_placeholders(module)?,
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
                    calls.append(&mut expr.calls())
                }

                calls
            }
            Self::Function { function, .. } => vec![Rc::clone(function)],
            Self::Grouping(expr) => expr.calls(),
            Self::Let { value, .. } => value.calls(),
            Self::List { expressions, .. } => {
                let mut calls = vec![];
                for expr in expressions {
                    calls.append(&mut expr.calls());
                }

                calls
            }
            Self::Literal { .. } => vec![],
            Self::Method(method) => method.body.calls(),
            Self::PlaceHolder(_) => internal_error!("called 'calls' on placeholder"),
            Self::Unary { right, .. } => right.calls(),
            Self::Variable { .. } => vec![],
        }
    }

    pub(crate) fn line(&self) -> usize {
        match self {
            Expr::Binary { right, .. } => right.line(),
            Expr::Block { body, line } => {
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
                    expr.line()
                } else {
                    *line
                }
            }
            Expr::Literal { line, .. } => *line,
            Expr::Method(method) => method.body.line(),
            Expr::PlaceHolder(placeholder) => placeholder.line(),
            Expr::Unary { right, .. } => right.line(),
            Expr::Variable { line, .. } => *line,
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
}

impl Binding {
    fn name(&self) -> String {
        match self {
            Binding::Function(f) => f.borrow().name().to_string(),
            Binding::Module(m) => m.borrow().name.clone(),
        }
    }
}

#[derive(Debug)]
struct Module {
    name: String,
    submodules: Vec<(Rc<RefCell<Module>>, bool)>,
    functions: Vec<Rc<RefCell<Function>>>,
    function_names: Vec<String>,
    bindings: BTreeMap<String, Binding>,
    supermodule: Weak<RefCell<Module>>,
}

impl Module {
    fn new(name: &str, bindings: BTreeMap<String, Binding>) -> Module {
        Module {
            name: name.to_string(),
            submodules: vec![],
            functions: vec![],
            function_names: vec![],
            bindings,
            supermodule: Weak::new(),
        }
    }

    fn find(&self, name: &str, looking_from_module_tree: bool) -> Option<Binding> {
        if let Some(module) = self.find_module(name, looking_from_module_tree) {
            Some(Binding::Module(module))
        } else {
            self.find_function(name).map(Binding::Function)
        }
    }

    fn find_module(
        &self,
        name: &str,
        looking_from_module_tree: bool,
    ) -> Option<Rc<RefCell<Module>>> {
        for (submodule, public) in &self.submodules {
            if submodule.borrow().name == name && (looking_from_module_tree || *public) {
                return Some(Rc::clone(submodule));
            }
        }

        None
    }

    fn find_function(&self, name: &str) -> Option<Rc<RefCell<Function>>> {
        for function in &self.functions {
            if function.borrow().name() == name {
                return Some(Rc::clone(function));
            }
        }

        None
    }
}

/// Returns the module, and the path to the file containing that code, if it exists
fn resolve_module(
    name: &str,
    importing_module: Rc<RefCell<Module>>,
    _modules: &mut [Rc<RefCell<Module>>],
    line: usize,
) -> Result<(Rc<RefCell<Module>>, bool)> {
    if let Some(module) = importing_module.borrow().find_module(name, true) {
        return Ok((module, true));
    }

    let location = format!(" at {name}");
    Err(Error::new(
        line,
        "Import",
        &location,
        "Cannot resolve module",
    ))
}

/// Returns the module, and the path to the file containing that code, if it exists
fn resolve_submodule(
    name: &str,
    line: usize,
    supermodule_origin: Option<&str>,
) -> Result<(String, String)> {
    #[cfg(not(feature = "no_std"))]
    if let Some(supermodule_origin) = supermodule_origin {
        let supermodule_origin = path::PathBuf::from(supermodule_origin);
        let supermodule_filename = supermodule_origin
            .file_name()
            .unwrap_or_else(|| internal_error!("set invalid module path"));
        let supermodule_folder = supermodule_origin
            .parent()
            .unwrap_or_else(|| internal_error!("set invalid module path"));
        if supermodule_filename == "mod.eri" {
            let mut module_file = supermodule_folder.to_owned();
            let file_name = path::PathBuf::from(format!("{name}.eri"));
            module_file.push(file_name);
            if fs::try_exists(&module_file).unwrap_or(false) {
                let contents = fs::read_to_string(&module_file)
                    .unwrap_or_else(|_| internal_error!("read from invalid file"));
                return Ok((contents, module_file.to_string_lossy().into()));
            }
            let mut module_in_folder = supermodule_folder.to_owned();
            let file_name = path::PathBuf::from(format!("{name}/mod.eri"));
            module_in_folder.push(file_name);
            if fs::try_exists(&module_file).unwrap_or(false) {
                let contents = fs::read_to_string(&module_file)
                    .unwrap_or_else(|_| internal_error!("read from invalid file"));
                return Ok((contents, module_in_folder.to_string_lossy().into()));
            }
        }
    }

    let location = format!(" at {name}");
    Err(Error::new(
        line,
        "Import",
        &location,
        "Cannot resolve submodule",
    ))
}

fn resolve_import(
    mut import: ImportTree,
    mut import_module: Rc<RefCell<Module>>,
    looking_from_module_tree: bool,
) -> Result<Binding> {
    let mut binding = Binding::Module(Rc::clone(&import_module));
    while let Some(next) = import.next() {
        let tmp: &ImportTree = next;
        import = tmp.clone();

        if import.name().kind() == TokenType::Super {
            let supermodule =
                import_module
                    .borrow()
                    .supermodule
                    .upgrade()
                    .unwrap_or(Err(Error::new(
                        import.line(),
                        "Import",
                        "",
                        "'super' used in top-level module",
                    ))?);
            binding = Binding::Module(Rc::clone(&supermodule));
            import_module = supermodule;
        } else {
            binding = match import_module
                .borrow()
                .find(import.name().lexeme(), looking_from_module_tree)
            {
                Some(binding) => binding,
                None => {
                    return Err(Error::new(
                        import.line(),
                        "Import",
                        &format!(" at '{}'", import.name().lexeme()),
                        "No such item",
                    ));
                }
            };

            if import.next().is_some() {
                if let Binding::Module(module) = &binding {
                    import_module = Rc::clone(module);
                } else if let Binding::Function(_) = &binding {
                    let location = format!(" at {}", import.name().lexeme());
                    return Err(Error::new(
                        import.line(),
                        "Import",
                        &location,
                        "Cannot import from function",
                    ));
                }
            }
        }
    }

    Ok(binding)
}

#[allow(clippy::type_complexity)]
fn get_std() -> Result<(Vec<Rc<RefCell<Module>>>, BTreeMap<String, Binding>)> {
    let mut eridani_std = super::eridani_std::ERIDANI_STD_BASIC.to_owned();

    #[cfg(feature = "target_std")]
    (eridani_std += super::eridani_std::ERIDANI_STD_FEATURE_STD);

    #[cfg(feature = "target_web")]
    (eridani_std += super::eridani_std::ERIDANI_STD_FEATURE_WEB);

    let natives_module = Rc::new(RefCell::new(Module::new("internals", BTreeMap::new())));
    for (name, func) in natives::NATIVES {
        natives_module
            .borrow_mut()
            .functions
            .push(Rc::new(RefCell::new(Function::Rust {
                name: name.to_string(),
                func: Box::new(func),
            })));
    }

    let std_module = Rc::new(RefCell::new(Module::new("std", BTreeMap::new())));
    std_module
        .borrow_mut()
        .submodules
        .push((Rc::clone(&natives_module), false));

    let mut modules = vec![Rc::clone(&std_module), Rc::clone(&natives_module)];

    let eridani_std = parser::parse(scanner::scan(&eridani_std)?)?;
    analyse_module(
        eridani_std,
        None,
        Rc::clone(&std_module),
        &mut modules,
        &BTreeMap::new(),
    )?;

    let prelude = super::eridani_std::PRELUDE;
    let mut bindings = BTreeMap::new();
    for function in std_module.borrow().functions.iter() {
        if prelude.contains(&function.borrow().name()) {
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

fn verify_pattern(pattern: Pattern, line: usize) -> Result<Pattern> {
    match pattern.clone() {
        Pattern::Binary {
            left,
            right,
            operator,
        } => {
            if (left.is_literal(&[]) || right.is_literal(&[])) && operator == value::LogOp::And {
                return Err(Error::new(
                    line,
                    "Pattern",
                    "",
                    "Patterns of the form '<literal> & <pattern>' are forbidden",
                ));
            }
            verify_pattern(*left, line)?;
            verify_pattern(*right, line)?;
        }
        Pattern::List { left, right } => {
            verify_pattern(*left, line)?;
            verify_pattern(*right, line)?;
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
        _ => {}
    }

    Ok(pattern)
}

pub fn analyse(
    parse_tree: ParseTree,
    source_origin: Option<&str>,
    entry_point: &str,
) -> Result<Program> {
    let (mut modules, default_bindings) = get_std()?;
    let root_module = Rc::new(RefCell::new(Module::new(
        "",
        clone_bindings(&default_bindings),
    )));
    modules.push(Rc::clone(&root_module));
    analyse_module(
        parse_tree,
        source_origin,
        Rc::clone(&root_module),
        &mut modules,
        &default_bindings,
    )?;

    let entry_point = match root_module.borrow().find_function(entry_point) {
        Some(entry_point) => entry_point,
        None => {
            let message = format!("Cannot find entry point '{entry_point}'");
            return Err(Error::new(1, "Name", "", &message));
        }
    };

    let mut calls = Calls::new();
    calls.push(Rc::clone(&entry_point));
    let functions = calls.calls;

    Ok(Program(entry_point, functions))
}

fn analyse_module(
    parse_tree: ParseTree,
    source_origin: Option<&str>,
    module: Rc<RefCell<Module>>,
    modules: &mut Vec<Rc<RefCell<Module>>>,
    bindings: &BTreeMap<String, Binding>,
) -> Result<()> {
    for (public, module_name) in parse_tree.modules() {
        let public = public.is_some();
        let (submodule, submodule_origin) =
            resolve_submodule(module_name.lexeme(), module_name.line(), source_origin)?;
        let submodule_parse_tree = parser::parse(scanner::scan(&submodule)?)?;
        let submodule = Rc::new(RefCell::new(Module::new(
            module_name.lexeme(),
            clone_bindings(bindings),
        )));
        modules.push(Rc::clone(&submodule));
        module
            .borrow_mut()
            .submodules
            .push((Rc::clone(&submodule), public));
        module.borrow_mut().bindings.insert(
            module_name.lexeme().to_string(),
            Binding::Module(Rc::clone(&submodule)),
        );
        analyse_module(
            submodule_parse_tree,
            Some(&submodule_origin),
            Rc::clone(&submodule),
            modules,
            bindings,
        )?;
    }

    for import in parse_tree.imports() {
        let (imported_module, in_module_tree) = resolve_module(
            import.name().lexeme(),
            Rc::clone(&module),
            modules,
            import.line(),
        )?;
        modules.push(Rc::clone(&imported_module));

        let binding = resolve_import(import.clone(), Rc::clone(&imported_module), in_module_tree)?;

        module.borrow_mut().bindings.insert(binding.name(), binding);
    }

    for function in parse_tree.functions() {
        module
            .borrow_mut()
            .function_names
            .push(function.name().lexeme().to_string())
    }

    for (i, function) in parse_tree.functions().iter().enumerate() {
        let name = module.borrow().function_names[i].clone();

        let mut methods = vec![];
        for method in function.methods() {
            let tmp: Vec<(Option<String>, Result<Pattern>)> = method
                .args()
                .iter()
                .map(|x| (x.name(), verify_pattern(x.pattern().into(), x.line())))
                .collect();
            let mut args = vec![];
            for (name, pattern) in tmp {
                args.push((name, pattern?))
            }

            let mut internal_bindings = vec![];
            for (name, pattern) in args.iter().map(|(a, b)| (a, b)) {
                if let Some(name) = name {
                    internal_bindings.push(name.clone());
                }
                internal_bindings.append(&mut pattern.bindings());
            }

            let (body, _) = convert_expr(
                method.body(),
                Rc::clone(&module),
                internal_bindings,
                modules,
            )?;

            methods.push(RefCell::new(Method { args, body }));
        }

        module
            .borrow_mut()
            .functions
            .push(Rc::new(RefCell::new(Function::Eridani { name, methods })))
    }

    for function in &module.borrow().functions {
        function.borrow().resolve_placeholders(&module)?;
    }

    Ok(())
}

fn convert_expr(
    expr: &parser::Expr,
    module: Rc<RefCell<Module>>,
    bindings: Vec<String>,
    modules: &mut [Rc<RefCell<Module>>],
) -> Result<(Expr, Vec<String>)> {
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

            let (left, bindings) = convert_expr(left, Rc::clone(&module), bindings, modules)?;
            let (right, bindings) = convert_expr(right, module, bindings, modules)?;

            (
                Expr::Binary {
                    left: Box::new(left),
                    operator,
                    right: Box::new(right),
                },
                bindings,
            )
        }
        ParseExpr::Block { body, end } => {
            let body = convert_exprs(body, &module, &bindings, modules)?;
            (
                Expr::Block {
                    body,
                    line: end.line(),
                },
                bindings,
            )
        }
        ParseExpr::Call {
            callee,
            paren,
            arguments,
        } => {
            let (callee, bindings) = convert_expr(callee, Rc::clone(&module), bindings, modules)?;
            let arguments = convert_exprs(arguments, &module, &bindings, modules)?;
            (
                Expr::Call {
                    callee: Box::new(callee),
                    arguments,
                    line: paren.line(),
                },
                bindings,
            )
        }
        ParseExpr::Grouping(expr) => {
            let (expr, bindings) = convert_expr(expr, Rc::clone(&module), bindings, modules)?;
            (Expr::Grouping(Box::new(expr)), bindings)
        }
        ParseExpr::Let { pattern, value } => {
            let (value, bindings) = convert_expr(value, module, bindings, modules)?;
            (
                Expr::Let {
                    pattern: pattern.into(),
                    value: Box::new(value),
                },
                bindings,
            )
        }
        ParseExpr::List { expressions, end } => (
            Expr::List {
                expressions: convert_exprs(expressions, &module, &bindings, modules)?,
                line: end.line(),
            },
            bindings,
        ),
        ParseExpr::Literal(token) => {
            let line = token.line();
            (
                Expr::Literal {
                    value: token.into(),
                    line,
                },
                bindings,
            )
        }
        ParseExpr::Method(method) => {
            let args: Vec<(Option<String>, Pattern)> = method
                .args()
                .iter()
                .map(|x| (x.name(), x.pattern().into()))
                .collect();

            let mut internal_bindings = bindings.clone();
            for (name, pattern) in args.iter().map(|(a, b)| (a, b)) {
                if let Some(name) = name {
                    internal_bindings.push(name.clone());
                }
                internal_bindings.append(&mut pattern.bindings());
            }

            let (body, _) = convert_expr(method.body(), module, internal_bindings, modules)?;
            (Expr::Method(Box::new(Method { args, body })), bindings)
        }
        ParseExpr::Unary { operator, right } => {
            let operator = match operator.kind() {
                TokenType::Minus => UnOp::Negate,
                _ => internal_error!("parsed token {:?} as unary", operator),
            };

            let (right, bindings) = convert_expr(right, module, bindings, modules)?;

            (
                Expr::Unary {
                    operator,
                    right: Box::new(right),
                },
                bindings,
            )
        }
        ParseExpr::Variable(var) => {
            let mut bindings = bindings;
            let expr = if var.next().is_some() {
                let (import_module, looking_from_module_tree) =
                    resolve_module(var.name().lexeme(), module, modules, var.name().line())?;
                let item = resolve_import(var.clone(), import_module, looking_from_module_tree)?;

                let function = match item {
                    Binding::Function(f) => f,
                    Binding::Module(module) => {
                        let location = format!(" at {}", module.borrow().name);
                        return Err(Error::new(
                            var.line(),
                            "Import",
                            &location,
                            "Expected function, found module",
                        ));
                    }
                };

                Expr::Function {
                    function,
                    line: var.line(),
                }
            } else if bindings.contains(var.name().lexeme()) {
                Expr::Variable {
                    name: var.name().lexeme().to_string(),
                    line: var.line(),
                }
            } else if module
                .borrow()
                .function_names
                .contains(&var.name().lexeme().to_string())
            {
                Expr::PlaceHolder(var.clone())
            } else if let Some(binding) = module.borrow().bindings.get(var.name().lexeme()) {
                if let Binding::Function(_) = binding {
                    Expr::PlaceHolder(var.clone())
                } else {
                    Expr::Variable {
                        name: var.name().lexeme().to_string(),
                        line: var.line(),
                    }
                }
            } else {
                let name = var.name().lexeme().to_string();
                bindings.push(name.clone());
                Expr::Variable {
                    name,
                    line: var.line(),
                }
            };

            (expr, bindings)
        }
    };

    Ok(result)
}

fn convert_exprs(
    values: &[parser::Expr],
    module: &Rc<RefCell<Module>>,
    bindings: &[String],
    modules: &mut [Rc<RefCell<Module>>],
) -> Result<Vec<Expr>> {
    let mut result = vec![];
    let mut expr;
    let mut inner_bindings = bindings.to_owned();
    for value in values {
        (expr, inner_bindings) = convert_expr(value, Rc::clone(module), inner_bindings, modules)?;
        result.push(expr);
    }
    Ok(result)
}
