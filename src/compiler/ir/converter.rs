use crate::{
    common::{internal_error, natives},
    compiler::{
        self,
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

use alloc::rc::Rc;
use alloc::rc::Weak;

use hashbrown::HashMap;

struct Calls {
    calls: Vec<Rc<RefCell<Function>>>,
}

impl Calls {
    const fn new() -> Self {
        Calls { calls: vec![] }
    }

    fn push(&mut self, function: &Rc<RefCell<Function>>) {
        if !self.calls.contains(function) {
            self.calls.push(Rc::clone(function));
            for call in function.borrow().calls() {
                self.push(&call);
            }
        }
    }
}

#[allow(clippy::type_complexity)]
fn get_std() -> Result<(Vec<Rc<RefCell<Module>>>, HashMap<String, Binding>)> {
    let mut eridani_std = compiler::eridani_std::ERIDANI_STD_BASIC.to_owned();

    #[cfg(feature = "target_std")]
    (eridani_std += compiler::eridani_std::ERIDANI_STD_FEATURE_STD);

    #[cfg(feature = "target_web")]
    (eridani_std += compiler::eridani_std::ERIDANI_STD_FEATURE_WEB);

    let std_module = Rc::new(RefCell::new(Module::new(
        "std",
        HashMap::new(),
        None,
        Weak::new(),
    )));

    let mut natives_module = Module::new(
        "internals",
        HashMap::new(),
        None,
        Rc::downgrade(&std_module),
    );
    for (name, func) in natives::NATIVES {
        natives_module.push_function(Rc::new(RefCell::new(Function::Rust {
            name: name.to_string(),
            func,
        })));
    }
    let natives_module = Rc::new(RefCell::new(natives_module));

    std_module
        .borrow_mut()
        .push_submodule(Rc::clone(&natives_module), Visibility::Private);

    let mut modules = vec![Rc::clone(&std_module), natives_module];

    let eridani_std = parser::parse(scanner::scan(&eridani_std)?, eridani_std)?;
    analyse_module(&eridani_std, Rc::clone(&std_module), &mut modules)?;

    let prelude = compiler::eridani_std::PRELUDE;
    let mut bindings = HashMap::new();
    bindings.insert("std".to_string(), Binding::Module(Rc::clone(&std_module)));
    for function in std_module.borrow().functions() {
        if prelude.contains(&function.borrow().name()) {
            bindings.insert(
                function.borrow().name().to_string(),
                Binding::Function(Rc::clone(function)),
            );
        }
    }

    Ok((modules, bindings))
}

pub(super) fn convert(
    parse_tree: &ParseTree,
    source_origin: Option<String>,
    entry_point: &str,
) -> Result<Program> {
    let (mut modules, default_bindings) = get_std()?;
    let root_module = Rc::new(RefCell::new(Module::new(
        "",
        clone_bindings(&default_bindings),
        source_origin,
        Weak::new(),
    )));
    modules.push(Rc::clone(&root_module));
    analyse_module(parse_tree, Rc::clone(&root_module), &mut modules)?;

    let Some(entry_point) = root_module.borrow().find_function(entry_point) else {
        let message = format!("Cannot find entry point '{entry_point}'");
        return Err(Error::new(1, "Name", "", &message));
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
    calls.push(&entry_point);
    let functions = calls.calls;

    Ok(Program::new(
        entry_point,
        functions,
        parse_tree.source().to_string(),
    ))
}

pub(super) fn analyse_module(
    parse_tree: &ParseTree,
    module: Rc<RefCell<Module>>,
    modules: &mut Vec<Rc<RefCell<Module>>>,
) -> Result<()> {
    module.borrow_mut().init_names(parse_tree);

    for (public, module_name) in parse_tree.modules() {
        let visibility = if public.is_some() {
            Visibility::Public
        } else {
            Visibility::Private
        };
        Module::resolve_submodule(
            Rc::clone(&module),
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

        let binding = resolve_import(import.clone(), imported_module, in_module_tree)?;

        module.borrow_mut().add_binding(binding.name(), binding);
    }

    for (i, function) in parse_tree.functions().iter().enumerate() {
        let name = module.borrow().function_name(i).to_owned();

        let mut methods = vec![];
        for method in function.methods() {
            let method = analyse_method(method, None, Rc::clone(&module), modules)?;

            methods.push(Rc::new(RefCell::new(method)));
        }

        methods.sort_by(|x, y| x.borrow().precedence.cmp(&y.borrow().precedence).reverse());

        module
            .borrow_mut()
            .push_function(Rc::new(RefCell::new(Function::Eridani { name, methods })));
    }

    Ok(())
}

fn analyse_method(
    method: &parser::Method,
    enclosing: Option<Rc<RefCell<Environment>>>,
    module: Rc<RefCell<Module>>,
    modules: &mut Vec<Rc<RefCell<Module>>>,
) -> Result<Method> {
    let mut environment = enclosing.map_or_else(Environment::new_toplevel, Environment::new);

    let mut lines = vec![];
    let mut args: Vec<(Option<&str>, Pattern)> = method
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

    let environment = Rc::new(RefCell::new(environment));

    let body = convert_expr(method.body(), module, Rc::clone(&environment), modules)?;

    Ok(Method {
        args,
        arg_order,
        body,
        environment,
        precedence,
    })
}

fn convert_variable(
    var: &parser::ImportTree,
    module: Rc<RefCell<Module>>,
    environment: &Environment,
) -> Result<Expr> {
    let expr = if var.next().is_some() {
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
        .iter()
        .any(|fun| fun == var.name().lexeme())
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
    } else {
        let location = format!(" at {}", var.name().lexeme());
        let message = format!("Unknown name '{}'", var.name().lexeme());
        return Err(Error::new(var.line(), "Name", &location, &message));
    };

    Ok(expr)
}

fn convert_expr(
    expr: &parser::Expr,
    module: Rc<RefCell<Module>>,
    environment: Rc<RefCell<Environment>>,
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

            let left = convert_expr(left, Rc::clone(&module), Rc::clone(&environment), modules)?;
            let right = convert_expr(right, module, environment, modules)?;

            Expr::Binary {
                left: Box::new(left),
                operator,
                right: Box::new(right),
            }
        }
        ParseExpr::Block { body, end } => {
            let inner_environment = Rc::new(RefCell::new(Environment::new(environment)));
            let body = convert_exprs(body, &module, &inner_environment, modules)?;

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
            let callee =
                convert_expr(callee, Rc::clone(&module), Rc::clone(&environment), modules)?;

            let arguments = convert_list(arguments, &module, &environment, modules)?;

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
            let value = convert_expr(value, module, Rc::clone(&environment), modules)?;
            let mut pattern: Pattern = pattern.into();
            pattern.bind(&mut environment.borrow_mut());

            Expr::Let {
                pattern,
                value: Box::new(value),
            }
        }
        ParseExpr::List { expressions, end } => Expr::List {
            expressions: convert_list(expressions, &module, &environment, modules)?,
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
        ParseExpr::Variable(var) => convert_variable(var, module, &environment.borrow())?,
    };

    Ok(result)
}

fn convert_exprs(
    values: &[parser::Expr],
    module: &Rc<RefCell<Module>>,
    environment: &Rc<RefCell<Environment>>,
    modules: &mut Vec<Rc<RefCell<Module>>>,
) -> Result<Vec<Expr>> {
    let mut result = vec![];
    for value in values {
        let expr = convert_expr(value, Rc::clone(module), Rc::clone(environment), modules)?;

        result.push(expr);
    }
    Ok(result)
}

fn convert_list(
    values: &[parser::Expr],
    module: &Rc<RefCell<Module>>,
    environment: &Rc<RefCell<Environment>>,
    modules: &mut Vec<Rc<RefCell<Module>>>,
) -> Result<Vec<ListExpr>> {
    let mut result = vec![];
    for value in values {
        let expr = convert_expr(value, Rc::clone(module), Rc::clone(environment), modules)?;
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
