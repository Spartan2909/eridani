#[cfg(feature = "std")]
mod ffi;
#[cfg(feature = "std")]
use ffi::{load_foreign_module, FOREIGN_MODULE_EXTENSION};

use core::{
    cell::{RefCell, RefMut},
    ptr::NonNull,
};

use alloc::{
    collections::BTreeMap,
    rc::{Rc, Weak},
};

#[cfg(feature = "std")]
use std::{
    fs,
    path::{self, PathBuf},
};

use crate::{
    common::internal_error,
    compiler::{
        ir::{clone_bindings, converter::analyse_module, Binding, Function, Library},
        parser::{self, ImportTree, ParseTree},
        scanner::{self, Token, TokenType},
        Error, Result,
    },
};

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(super) enum Visibility {
    Public,
    Private,
}

#[cfg(feature = "std")]
enum ModuleFile {
    Foreign(PathBuf),
    Source(String, PathBuf),
}

#[derive(Debug)]
pub(super) struct Module {
    name: String,
    submodules: Vec<(Rc<RefCell<Module>>, Visibility)>,
    functions: Vec<Rc<RefCell<Function>>>,
    function_names: Vec<String>,
    bindings: BTreeMap<String, Binding>,
    supermodule: Weak<RefCell<Module>>,
    origin: Option<String>,
    library: Option<NonNull<Library>>,
}

impl PartialEq for Module {
    fn eq(&self, other: &Self) -> bool {
        self.name.as_ptr() as usize == other.name.as_ptr() as usize
    }
}

impl Module {
    pub fn new(
        name: &str,
        bindings: BTreeMap<String, Binding>,
        origin: Option<String>,
        supermodule: Option<&Rc<RefCell<Module>>>,
    ) -> Module {
        Module {
            name: name.to_string(),
            submodules: vec![],
            functions: vec![],
            function_names: vec![],
            bindings,
            supermodule: supermodule.map(Rc::downgrade).unwrap_or(Weak::new()),
            origin,
            library: None,
        }
    }

    pub fn name(&self) -> &String {
        &self.name
    }

    pub fn functions(&self) -> &Vec<Rc<RefCell<Function>>> {
        &self.functions
    }

    pub fn bindings(&self) -> &BTreeMap<String, Binding> {
        &self.bindings
    }

    pub fn push_function(&mut self, function: Rc<RefCell<Function>>) {
        self.functions.push(function);
    }

    pub fn push_submodule(&mut self, submodule: Rc<RefCell<Module>>, visibility: Visibility) {
        self.submodules.push((submodule, visibility));
    }

    pub fn add_binding(&mut self, name: String, binding: Binding) {
        self.bindings.insert(name, binding);
    }

    pub fn init_names(&mut self, parse_tree: &ParseTree) {
        for function in parse_tree.functions() {
            self.function_names
                .push(function.name().lexeme().to_string());
        }
    }

    pub fn function_name(&self, index: usize) -> &String {
        &self.function_names[index]
    }

    pub fn function_names(&self) -> &Vec<String> {
        &self.function_names
    }

    pub fn resolve_placeholders(&self) -> Result<()> {
        for function in &self.functions {
            function.borrow().resolve_placeholders()?;
        }

        Ok(())
    }

    pub fn get_binding(&self, name: &str, looking_from_module_tree: bool) -> Option<Binding> {
        if let Some(module) = self.find_submodule(name, looking_from_module_tree) {
            Some(Binding::Module(Rc::clone(module)))
        } else {
            self.find_function(name).map(Binding::Function)
        }
    }

    fn find_submodule(
        &self,
        name: &str,
        looking_from_module_tree: bool,
    ) -> Option<&Rc<RefCell<Module>>> {
        for (submodule, visibility) in &self.submodules {
            if submodule.borrow().name == name
                && (looking_from_module_tree || *visibility == Visibility::Public)
            {
                return Some(submodule);
            }
        }

        None
    }

    pub fn find_function(&self, name: &str) -> Option<Rc<RefCell<Function>>> {
        for function in &self.functions {
            if function.borrow().name() == name {
                return Some(Rc::clone(function));
            }
        }

        None
    }

    pub fn destroy(&mut self) {
        for (submodule, _) in &self.submodules {
            if let Ok(mut submodule) = submodule.try_borrow_mut() {
                submodule.destroy();
            }
        }
        self.submodules = vec![];
        self.functions = vec![];
        for binding in self.bindings.values() {
            if let Binding::Module(module) = binding {
                if let Ok(mut module) = module.try_borrow_mut() {
                    module.destroy();
                }
            }
        }
        self.bindings.clear();
    }

    /// Returns the module and whether that module is in the current module tree
    pub fn find_imported_module(&self, name: &Token) -> Result<(Rc<RefCell<Module>>, bool)> {
        if name.kind() == TokenType::Super {
            if let Some(supermodule) = self.supermodule.upgrade() {
                return Ok((supermodule, true));
            } else {
                return Err(Error::new(
                    name.line(),
                    "Import",
                    "",
                    "'super' used in top-level module",
                ));
            }
        } else if let Some(module) = self.find_submodule(name.lexeme(), true) {
            return Ok((Rc::clone(module), true));
        } else if let Some(Binding::Module(module)) = self.bindings.get(name.lexeme()) {
            return Ok((Rc::clone(module), false));
        }

        let location = format!(" at '{}'", name.lexeme());
        Err(Error::new(
            name.line(),
            "Import",
            &location,
            "Cannot resolve module",
        ))
    }

    fn find_submodule_file(&self, name: &str) -> Option<ModuleFile> {
        #[cfg(not(feature = "no_std"))]
        if let Some(supermodule_origin) = &self.origin {
            let supermodule_origin = path::PathBuf::from(supermodule_origin);
            let supermodule_filename = supermodule_origin
                .file_name()
                .unwrap_or_else(|| internal_error!("set invalid module path"))
                .to_str()
                .unwrap_or_else(|| internal_error!("set invalid module path"));
            let supermodule_folder = supermodule_origin
                .parent()
                .unwrap_or_else(|| internal_error!("set invalid module path"));

            let paths_to_try = if supermodule_filename == "mod.eri" {
                [format!("{name}.eri"), format!("{name}/mod.eri")]
            } else {
                let module_name = supermodule_filename
                    .rsplit_once('.')
                    .unwrap_or_else(|| internal_error!("source file without extension"))
                    .0;
                [
                    format!("{module_name}/{name}.eri"),
                    format!("{module_name}/{name}/mod.eri"),
                ]
            };

            for path in paths_to_try {
                let mut module_file = supermodule_folder.to_owned();
                module_file.push(path);
                if module_file.try_exists().unwrap_or(false) {
                    let contents = fs::read_to_string(&module_file)
                        .unwrap_or_else(|_| internal_error!("read from invalid file"));
                    return Some(ModuleFile::Source(contents, module_file));
                }
            }

            let paths_to_try = if supermodule_filename == "mod.eri" {
                [
                    format!("{name}.{FOREIGN_MODULE_EXTENSION}"),
                    format!("{name}/mod.{FOREIGN_MODULE_EXTENSION}"),
                ]
            } else {
                let module_name = supermodule_filename
                    .rsplit_once('.')
                    .unwrap_or_else(|| internal_error!("source file without extension"))
                    .0;
                [
                    format!("{module_name}/{name}.{FOREIGN_MODULE_EXTENSION}"),
                    format!("{module_name}/{name}/mod.{FOREIGN_MODULE_EXTENSION}"),
                ]
            };

            for path in paths_to_try {
                let mut module_file = supermodule_folder.to_owned();
                module_file.push(path);
                if module_file.try_exists().unwrap_or(false) {
                    return Some(ModuleFile::Foreign(module_file));
                }
            }
        }

        None
    }

    pub fn resolve_submodule(
        this: &Rc<RefCell<Module>>,
        name: &str,
        line: usize,
        modules: &mut Vec<Rc<RefCell<Module>>>,
        visibility: Visibility,
    ) -> Result<Rc<RefCell<Module>>> {
        // Storing the possible source in a variable prior to the `if let` allows the
        // `Ref` to be dropped before subsequent calls to `borrow_mut()`
        let possible_source = this.borrow().find_submodule_file(name);
        if let Some(module_file) = possible_source {
            match module_file {
                ModuleFile::Foreign(path) => load_foreign_module(path, name, this, line),
                ModuleFile::Source(source, path) => {
                    let submodule_parse_tree = parser::parse(scanner::scan(source)?)?;
                    let submodule = Rc::new(RefCell::new(Module::new(
                        name,
                        clone_bindings(&this.borrow().bindings),
                        Some(path.to_string_lossy().into()),
                        Some(this),
                    )));
                    modules.push(Rc::clone(&submodule));
                    this.borrow_mut()
                        .push_submodule(Rc::clone(&submodule), visibility);
                    this.borrow_mut()
                        .add_binding(name.to_string(), Binding::Module(Rc::clone(&submodule)));
                    analyse_module(submodule_parse_tree, &submodule, modules)?;

                    Ok(submodule)
                }
            }
        } else {
            let location = format!(" at {name}");
            Err(Error::new(
                line,
                "Import",
                &location,
                "Cannot resolve submodule",
            ))
        }
    }
}

impl Drop for Module {
    fn drop(&mut self) {
        if let Some(library) = self.library {
            // SAFETY: `library` is the only pointer to this value
            unsafe { Box::from_raw(library.as_ptr()) };
        }

        self.destroy();
    }
}

pub(super) fn get_library(mut module: RefMut<Module>) -> Option<NonNull<Library>> {
    if let Some(library) = module.library {
        module.library = None;
        Some(library)
    } else {
        None
    }
}

pub(super) fn resolve_import(
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
                .get_binding(import.name().lexeme(), looking_from_module_tree)
            {
                Some(binding) => binding,
                None => {
                    if import_module
                        .borrow()
                        .function_names
                        .contains(import.name().lexeme())
                    {
                        Binding::PlaceHolder(
                            import.name().lexeme().to_owned(),
                            Rc::clone(&import_module),
                        )
                    } else {
                        return Err(Error::new(
                            import.line(),
                            "Import",
                            &format!(" at '{}'", import.name().lexeme()),
                            "No such item",
                        ));
                    }
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
