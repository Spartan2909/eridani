use crate::{
    common::{internal_error, natives},
    compiler::{
        self,
        arena::Arena,
        ir::{
            checker, clone_bindings,
            match_engine::resolve_metapatterns,
            modules::{resolve_import, Module, Visibility},
            BinOp, Binding, Environment, Expr, Function, ListExpr, Method, Pattern,
            PlaceHolderModule, Program, UnOp,
        },
        parser::{self, ParseTree},
        scanner::{self, TokenType},
        Error, Result,
    },
    prelude::*,
};

use core::cell::RefCell;

use alloc::collections::BTreeMap;

struct Calls<'arena> {
    calls: Vec<&'arena RefCell<Function<'arena>>>,
}

impl<'arena> Calls<'arena> {
    fn new() -> Self {
        Calls { calls: vec![] }
    }

    fn push(&mut self, function: &'arena RefCell<Function<'arena>>) {
        if !self.calls.contains(&function) {
            self.calls.push(function);
            for call in function.borrow().calls() {
                self.push(call);
            }
        }
    }
}

#[allow(clippy::type_complexity)]
fn get_std<'arena>(
    arena: &'arena Arena,
) -> Result<(
    Vec<&'arena RefCell<Module<'arena>>>,
    BTreeMap<String, Binding<'arena>>,
)> {
    let mut eridani_std = compiler::eridani_std::ERIDANI_STD_BASIC.to_owned();

    #[cfg(feature = "target_std")]
    (eridani_std += compiler::eridani_std::ERIDANI_STD_FEATURE_STD);

    #[cfg(feature = "target_web")]
    (eridani_std += compiler::eridani_std::ERIDANI_STD_FEATURE_WEB);

    let std_module: &'arena RefCell<Module<'arena>> = &*arena.allocate(RefCell::new(Module::new(
        "std",
        BTreeMap::new(),
        None,
        None,
    )));

    let natives_module = arena.allocate(RefCell::new(Module::new(
        "internals",
        BTreeMap::new(),
        None,
        Some(std_module),
    )));
    for (name, func) in natives::NATIVES {
        natives_module
            .get_mut()
            .push_function(arena.allocate(RefCell::new(Function::Rust {
                name: name.to_string(),
                func,
            })));
    }

    std_module
        .borrow_mut()
        .push_submodule(natives_module, Visibility::Private);

    let mut modules = vec![std_module, natives_module];

    let eridani_std = parser::parse(scanner::scan(eridani_std)?)?;
    analyse_module(arena, eridani_std, std_module, &mut modules)?;

    let prelude = compiler::eridani_std::PRELUDE;
    let mut bindings = BTreeMap::new();
    bindings.insert("std".to_string(), Binding::Module(std_module));
    for function in std_module.borrow().functions() {
        if prelude.contains(&function.borrow().name().as_str()) {
            bindings.insert(
                function.borrow().name().to_string(),
                Binding::Function(function),
            );
        }
    }

    Ok((modules, bindings))
}

pub(super) fn convert<'arena>(
    arena: &'arena Arena,
    parse_tree: ParseTree,
    source_origin: Option<String>,
    entry_point: &str,
) -> Result<Program<'arena>> {
    let (mut modules, default_bindings) = get_std(arena)?;
    let root_module = arena.allocate(RefCell::new(Module::new(
        "",
        clone_bindings(&default_bindings),
        source_origin,
        None,
    )));
    modules.push(root_module);
    analyse_module(arena, parse_tree, root_module, &mut modules)?;

    let entry_point = match root_module.borrow().find_function(entry_point) {
        Some(entry_point) => entry_point,
        None => {
            let message = format!("Cannot find entry point '{entry_point}'");
            return Err(Error::new(1, "Name", "", &message));
        }
    };

    let mut all_functions: Vec<&'arena RefCell<Function>> = vec![];

    for module in &modules {
        module.borrow().resolve_placeholders()?;
        for function in module.borrow().functions() {
            if !all_functions
                .iter()
                .any(|f| f.as_ptr() as usize == function.as_ptr() as usize)
            {
                all_functions.push(*function);
            }
        }
    }

    let mut calls = Calls::new();
    calls.push(entry_point);
    let functions = calls.calls;

    for function in all_functions {
        if !functions.contains(&function) {
            function.borrow_mut().destroy();
        }
    }

    Ok(Program::new(entry_point, functions))
}

pub(super) fn analyse_module<'arena>(
    arena: &'arena Arena,
    parse_tree: ParseTree,
    module: &'arena RefCell<Module<'arena>>,
    modules: &mut Vec<&'arena RefCell<Module<'arena>>>,
) -> Result<()> {
    module.borrow_mut().init_names(&parse_tree);

    for (public, module_name) in parse_tree.modules() {
        let visibility = if public.is_some() {
            Visibility::Public
        } else {
            Visibility::Private
        };
        Module::resolve_submodule(
            arena,
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
        modules.push(imported_module);

        let binding = resolve_import(import.clone(), imported_module, in_module_tree)?;

        module.borrow_mut().add_binding(binding.name(), binding);
    }

    for (i, function) in parse_tree.functions().iter().enumerate() {
        let name = module.borrow().function_name(i).clone();

        let mut methods = vec![];
        for method in function.methods() {
            let method = analyse_method(method, None, module, modules)?;

            methods.push(&*arena.allocate(RefCell::new(method)));
        }

        methods.sort_by(|x, y| x.borrow().precedence.cmp(&y.borrow().precedence).reverse());

        module
            .borrow_mut()
            .push_function(arena.allocate(RefCell::new(Function::Eridani { name, methods })));
    }

    Ok(())
}

#[allow(clippy::borrowed_box)] // This is required for safety
fn analyse_method<'arena>(
    method: &parser::Method,
    enclosing: Option<&Box<Environment>>,
    module: &'arena RefCell<Module<'arena>>,
    modules: &mut Vec<&'arena RefCell<Module<'arena>>>,
) -> Result<Method<'arena>> {
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
    let mut ordered_references = Vec::with_capacity(args.len());
    let mut precedence = 0;
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
        precedence += pattern.precedence();
    }

    for &index in &arg_order {
        let (_, pattern) = &args[index];
        let resolved_names: Vec<u16> = ordered_references[..=index]
            .iter()
            .flatten()
            .map(|&x| x.0)
            .collect();
        checker::verify_pattern(pattern, lines[index], &resolved_names)?;
    }

    let args = args
        .into_iter()
        .zip(references)
        .map(|((_, pattern), reference)| (reference, pattern))
        .collect();

    let body = convert_expr(method.body(), module, &mut environment, modules)?;

    Ok(Method {
        args,
        arg_order,
        body,
        environment,
        precedence,
    })
}

fn convert_expr<'arena>(
    expr: &parser::Expr,
    module: &'arena RefCell<Module<'arena>>,
    environment: &mut Box<Environment>,
    modules: &mut Vec<&'arena RefCell<Module<'arena>>>,
) -> Result<Expr<'arena>> {
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

            let left = convert_expr(left, module, environment, modules)?;
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
            let body = convert_exprs(body, module, &mut inner_environment, modules)?;

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
            let callee = convert_expr(callee, module, environment, modules)?;

            let arguments = convert_list(arguments, module, environment, modules)?;

            Expr::Call {
                callee: Box::new(callee),
                arguments,
                line: paren.line(),
            }
        }
        ParseExpr::Grouping(expr) => {
            let expr = convert_expr(expr, module, environment, modules)?;
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
            expressions: convert_list(expressions, module, environment, modules)?,
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
            let method = analyse_method(method, None, module, modules)?;

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
                        function,
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

fn convert_exprs<'arena>(
    values: &[parser::Expr],
    module: &'arena RefCell<Module<'arena>>,
    environment: &mut Box<Environment>,
    modules: &mut Vec<&'arena RefCell<Module<'arena>>>,
) -> Result<Vec<Expr<'arena>>> {
    let mut result = vec![];
    for value in values {
        let expr = convert_expr(value, module, environment, modules)?;

        result.push(expr);
    }
    Ok(result)
}

fn convert_list<'arena>(
    values: &[parser::Expr],
    module: &'arena RefCell<Module<'arena>>,
    environment: &mut Box<Environment>,
    modules: &mut Vec<&'arena RefCell<Module<'arena>>>,
) -> Result<Vec<ListExpr<'arena>>> {
    let mut result = vec![];
    for value in values {
        let expr = convert_expr(value, module, environment, modules)?;
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
