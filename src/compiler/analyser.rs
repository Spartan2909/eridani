use crate::{
    common::{
        value::{Pattern, Value},
        EridaniFunction,
    },
    compiler::{
        internal_error,
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
use core::{cell::RefCell, fmt::Debug, iter};

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

                for (method1, method2) in iter::zip(methods1, methods2) {
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

    fn methods(&self) -> Vec<RefCell<Method>> {
        match self {
            Function::Eridani { methods, .. } => methods.to_vec(),
            Function::Rust { .. } => vec![],
        }
    }
}

pub type Program = (Rc<RefCell<Function>>, Vec<Rc<RefCell<Function>>>);

#[derive(Debug, Clone, PartialEq)]
pub struct Method {
    args: Vec<(Option<String>, Pattern)>,
    body: Expr,
}

impl Method {
    pub fn args(&self) -> &Vec<(Option<String>, Pattern)> {
        &self.args
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
    Function(Rc<RefCell<Function>>),
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
    Variable(String),
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
            Self::Function(_) => {}
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
                    let function = if let Some(function) = module
                        .borrow()
                        .find_function(placeholder.name().lexeme()) {
                            function
                        } else if let Some(binding) = module.borrow().bindings.get(placeholder.name().lexeme()) {
                            if let Binding::Function(f) = binding {
                                f.clone()
                            } else {
                                internal_error!("placeholder pointing to module")
                            }
                        } else {
                            internal_error!("dangling placeholder")
                        };
                        

                    *self = Self::Function(function);
                }
            }
            Self::Unary { right, .. } => right.resolve_placeholders(module)?,
            Self::Variable(_) => {}
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
            Self::Function(function) => vec![Rc::clone(function)],
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
            Self::Variable(_) => vec![],
        }
    }
}

#[derive(Debug)]
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
    submodules: Vec<Rc<RefCell<Module>>>,
    functions: Vec<Rc<RefCell<Function>>>,
    function_names: Vec<String>,
    bindings: BTreeMap<String, Binding>,
    supermodule: Weak<RefCell<Module>>,
}

impl Module {
    fn new(name: &str) -> Module {
        Module {
            name: name.to_string(),
            submodules: vec![],
            functions: vec![],
            function_names: vec![],
            bindings: BTreeMap::new(),
            supermodule: Weak::new(),
        }
    }

    fn find(&self, name: &str) -> Option<Binding> {
        if let Some(module) = self.find_module(name) {
            Some(Binding::Module(module))
        } else if let Some(function) = self.find_function(name) {
            Some(Binding::Function(function))
        } else {
            None
        }
    }

    fn find_module(&self, name: &str) -> Option<Rc<RefCell<Module>>> {
        for submodule in &self.submodules {
            if submodule.borrow().name == name {
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
    _modules: &mut Vec<Rc<RefCell<Module>>>,
    line: usize,
) -> Result<Rc<RefCell<Module>>> {
    if let Some(module) = importing_module.borrow().find_module(name) {
        return Ok(module);
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
            binding = match import_module.borrow().find(import.name().lexeme()) {
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

pub fn analyse(
    parse_tree: ParseTree,
    source_origin: Option<&str>,
    entry_point: &str,
) -> Result<Program> {
    let root_module = Rc::new(RefCell::new(Module::new("")));
    let mut modules = vec![Rc::clone(&root_module)];
    analyse_module(
        parse_tree,
        source_origin,
        Rc::clone(&root_module),
        &mut modules,
    )?;

    let entry_point = match root_module.borrow().find_function(entry_point) {
        Some(entry_point) => entry_point,
        None => {
            let message = format!("Cannot find entry point '{entry_point}'");
            return Err(Error::new(1, "Name", "", &message));
        }
    };

    let mut functions = vec![];
    for module in modules {
        functions.append(&mut module.borrow_mut().functions)
    }

    let mut calls = vec![];
    for function in &functions {
        for method in function.borrow().methods() {
            for call in method.borrow().body.calls() {
                if !calls.contains(&call) && &call != function {
                    calls.push(call);
                }
            }
        }
    }

    let mut to_remove = vec![];
    for (i, function) in functions.iter().enumerate() {
        if !calls.contains(function) {
            to_remove.push(i);
        }
    }

    for (removed, index) in to_remove.iter().enumerate() {
        functions.remove(*index - removed);
    }

    functions.push(Rc::clone(&entry_point));

    Ok((entry_point, functions))
}

fn analyse_module(
    parse_tree: ParseTree,
    source_origin: Option<&str>,
    module: Rc<RefCell<Module>>,
    modules: &mut Vec<Rc<RefCell<Module>>>,
) -> Result<()> {
    for module_name in parse_tree.modules() {
        let (submodule, submodule_origin) =
            resolve_submodule(module_name.lexeme(), module_name.line(), source_origin)?;
        let submodule_parse_tree = parser::parse(scanner::scan(&submodule)?)?;
        let submodule = Rc::new(RefCell::new(Module::new(module_name.lexeme())));
        modules.push(Rc::clone(&submodule));
        module.borrow_mut().submodules.push(Rc::clone(&submodule));
        module.borrow_mut().bindings.insert(
            module_name.lexeme().to_string(),
            Binding::Module(Rc::clone(&submodule)),
        );
        analyse_module(
            submodule_parse_tree,
            Some(&submodule_origin),
            Rc::clone(&submodule),
            modules,
        )?;
    }

    for import in parse_tree.imports() {
        let imported_module = resolve_module(
            import.name().lexeme(),
            Rc::clone(&module),
            modules,
            import.line(),
        )?;
        modules.push(Rc::clone(&imported_module));

        let binding = resolve_import(import.clone(), Rc::clone(&imported_module))?;

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
            let mut args = vec![];

            for arg in method.args() {
                args.push((arg.name(), arg.pattern().into()))
            }

            let (body, _) = convert_expr(method.body(), Rc::clone(&module), vec![])?;

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

            let (left, bindings) = convert_expr(left, Rc::clone(&module), bindings)?;
            let (right, bindings) = convert_expr(right, module, bindings)?;

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
            let body = convert_exprs(body, &module, &bindings)?;
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
            let (callee, bindings) = convert_expr(callee, Rc::clone(&module), bindings)?;
            let arguments = convert_exprs(arguments, &module, &bindings)?;
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
            let (expr, bindings) = convert_expr(expr, Rc::clone(&module), bindings)?;
            (Expr::Grouping(Box::new(expr)), bindings)
        }
        ParseExpr::Let { pattern, value } => {
            let (value, bindings) = convert_expr(value, module, bindings)?;
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
                expressions: convert_exprs(expressions, &module, &bindings)?,
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
            let args = method
                .args()
                .iter()
                .map(|x| (x.name(), x.pattern().into()))
                .collect();
            let (body, _) = convert_expr(method.body(), module, bindings.clone())?;
            (Expr::Method(Box::new(Method { args, body })), bindings)
        }
        ParseExpr::Unary { operator, right } => {
            let operator = match operator.kind() {
                TokenType::Minus => UnOp::Negate,
                _ => internal_error!("parsed token {:?} as unary", operator),
            };

            let (right, bindings) = convert_expr(right, module, bindings)?;

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
                let item = resolve_import(var.clone(), module)?;

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

                Expr::Function(function)
            } else if bindings.contains(var.name().lexeme()) {
                Expr::Variable(var.name().lexeme().to_string())
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
                    Expr::Variable(var.name().lexeme().to_string())
                }
            } else {
                let name = var.name().lexeme().to_string();
                bindings.push(name.clone());
                Expr::Variable(name)
            };

            (expr, bindings)
        }
    };

    Ok(result)
}

fn convert_exprs(
    values: &[parser::Expr],
    module: &Rc<RefCell<Module>>,
    bindings: &Vec<String>,
) -> Result<Vec<Expr>> {
    let mut result = vec![];
    let mut expr;
    let mut inner_bindings = bindings.clone();
    for value in values {
        (expr, inner_bindings) = convert_expr(value, Rc::clone(module), inner_bindings)?;
        result.push(expr);
    }
    Ok(result)
}
