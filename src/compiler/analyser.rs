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

use alloc::{collections::BTreeMap, rc::Rc};
use core::{cell::RefCell, fmt::Debug};

pub enum Function {
    Eridani {
        name: String,
        methods: Vec<Method>,
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

impl Function {
    pub fn name(&self) -> &str {
        match self {
            Function::Eridani { name, .. } => name,
            Function::Rust { name, .. } => name,
        }
    }
}

#[derive(Debug, Clone)]
pub struct Method {
    args: Vec<(Option<String>, Pattern)>,
    body: Expr,
}

#[derive(Debug, Clone, Copy)]
pub enum BinOp {
    Add,
    Sub,
    Mul,
    Div,
    Mod,
}

#[derive(Debug, Clone, Copy)]
pub enum UnOp {
    Negate,
}

fn vec_convert(value: Vec<parser::Expr>) -> Vec<Expr> {
    value.into_iter().map(|x| x.into()).collect::<Vec<Expr>>()
}

#[derive(Debug, Clone)]
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
    Unary {
        operator: UnOp,
        right: Box<Self>,
    },
    //Variable(ImportTree),
}

impl From<parser::Expr> for Expr {
    fn from(value: parser::Expr) -> Self {
        use parser::Expr as ParseExpr;

        match value {
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

                Self::Binary {
                    left: Box::new((&*left).into()),
                    operator,
                    right: Box::new((&*right).into()),
                }
            }
            ParseExpr::Block { body, end } => Self::Block {
                body: vec_convert(body),
                line: end.line(),
            },
            ParseExpr::Call {
                callee,
                paren,
                arguments,
            } => Self::Call {
                callee: Box::new((&*callee).into()),
                arguments: vec_convert(arguments),
                line: paren.line(),
            },
            ParseExpr::Grouping(expr) => Self::Grouping(Box::new((&*expr).into())),
            ParseExpr::Let { pattern, value } => Self::Let {
                pattern: pattern.into(),
                value: Box::new((&*value).into()),
            },
            ParseExpr::List { expressions, end } => Self::List {
                expressions: vec_convert(expressions),
                line: end.line(),
            },
            ParseExpr::Literal(token) => {
                let line = token.line();
                Self::Literal {
                    value: token.into(),
                    line,
                }
            }
            ParseExpr::Method(method) => {
                let args: Vec<(Option<String>, Pattern)> = method
                    .args()
                    .iter()
                    .map(|x| (x.name(), x.pattern().into()))
                    .collect();
                let body: Expr = method.body().into();
                Expr::Method(Box::new(Method { args, body }))
            }
            ParseExpr::Unary { operator, right } => {
                let operator = match operator.kind() {
                    TokenType::Minus => UnOp::Negate,
                    _ => internal_error!("parsed token {:?} as unary", operator),
                };

                Self::Unary {
                    operator,
                    right: Box::new((&*right).into()),
                }
            }
            ParseExpr::Variable(var) => todo!(),
        }
    }
}

impl From<&parser::Expr> for Expr {
    fn from(value: &parser::Expr) -> Self {
        value.clone().into()
    }
}

#[derive(Debug)]
enum Binding {
    Function(Rc<RefCell<Function>>),
    Module(Rc<RefCell<Module>>),
}

#[derive(Debug)]
struct Module {
    name: String,
    submodules: Vec<Rc<RefCell<Module>>>,
    functions: Vec<Rc<RefCell<Function>>>,
    bindings: BTreeMap<String, Binding>,
}

impl Module {
    fn new(name: &str) -> Module {
        Module {
            name: name.to_string(),
            submodules: vec![],
            functions: vec![],
            bindings: BTreeMap::new(),
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
            if &submodule.borrow().name == name {
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

    fn functions(&self) -> Vec<Rc<RefCell<Function>>> {
        let mut functions = self.functions.clone();
        for module in &self.submodules {
            functions.extend(module.borrow().functions());
        }

        functions
    }
}

#[derive(Debug)]
struct Analyser {
    modules: Vec<Rc<RefCell<Module>>>,
}

fn find_module(name: &str, line: usize) -> Result<String> {
    let location = format!(" at {name}");
    Err(Error::new(
        line,
        "Import",
        &location,
        "Cannot resolve module name",
    ))
}

impl Analyser {
    fn new() -> Analyser {
        Analyser { modules: vec![] }
    }

    fn analyse(
        &mut self,
        parse_tree: ParseTree,
        entry_point: &str,
    ) -> Result<(Rc<RefCell<Function>>, Vec<Rc<RefCell<Function>>>)> {
        let root_module = Rc::new(RefCell::new(Module::new("")));
        self.modules.push(Rc::clone(&root_module));
        self.analyse_module(parse_tree, Rc::clone(&root_module))?;

        let entry = root_module.borrow().find_function(entry_point);
        let entry_point = if let Some(entry_point) = entry {
            entry_point
        } else {
            let message = format!("Cannot find entry point '{entry_point}'");
            return Err(Error::new(1, "Name", "", &message));
        };

        let functions = root_module.borrow().functions();
        Ok((entry_point, functions))
    }

    fn analyse_module(&self, parse_tree: ParseTree, module: Rc<RefCell<Module>>) -> Result<()> {
        for import in parse_tree.imports() {
            let module_parse_tree = parser::parse(scanner::scan(&find_module(
                import.name().lexeme(),
                import.line(),
            )?)?)?;
            let submodule = Rc::new(RefCell::new(Module::new(import.name().lexeme())));
            module.borrow_mut().submodules.push(Rc::clone(&submodule));
            self.analyse_module(module_parse_tree, Rc::clone(&submodule))?;

            let mut import = import.clone();
            let mut import_module = Rc::clone(&submodule);
            let mut binding = Binding::Module(Rc::clone(&import_module));
            loop {
                if let Some(next) = import.next() {
                    let tmp: &ImportTree = &next;
                    import = tmp.clone();

                    binding = import_module
                        .borrow()
                        .find(import.name().lexeme())
                        .unwrap_or(Err(Error::new(
                            import.line(),
                            "Import",
                            &format!(" at '{}'", import.name().lexeme()),
                            "No such item",
                        ))?);

                    if import.next().is_some() {
                        if let Binding::Module(module) = &binding {
                            import_module = Rc::clone(module);
                        } else if let Binding::Function(_) = &binding {
                            let location = format!(" at {}", import.name().lexeme());
                            return Err(Error::new(import.line(), "Import", &location, "Cannot import from function"));
                        }
                    }
                } else {
                    break;
                }
            }

            submodule.borrow_mut().bindings.insert(import.name().lexeme().to_string(), binding);
        }

        Ok(())
    }
}

pub fn analyse(
    parse_tree: ParseTree,
    entry_point: &str,
) -> Result<(Rc<RefCell<Function>>, Vec<Rc<RefCell<Function>>>)> {
    Analyser::new().analyse(parse_tree, entry_point)
}
