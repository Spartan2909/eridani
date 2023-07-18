use core::{
    cell::RefCell,
    ffi::{c_char, c_int, c_void, CStr},
    ptr::NonNull,
};

use alloc::{collections::BTreeMap, rc::Rc};

use std::path::PathBuf;

use crate::{
    common::{value::Value, ArgumentError, EridaniFunction},
    compiler::{
        self,
        ir::{Function, Module},
        Error,
    },
};

use libloading::{Library, Symbol};

#[inline]
fn load_error(line: usize) -> Error {
    Error::new(line, "Module", "", "Failed to load module")
}

#[repr(C)]
struct ForeignFunctionArray {
    functions: *const NamedFunction,
    number: c_int,
}

#[repr(C)]
#[derive(Clone, Copy)]
struct NamedFunction {
    function: ForeignFunction,
    name: *const c_char,
}

type ForeignFunction = unsafe extern "C" fn(c_int, *mut c_void) -> *mut c_void;

#[cfg(target_os = "linux")]
pub(super) const FOREIGN_MODULE_EXTENSION: &str = ".so";

#[cfg(windows)]
pub(super) const FOREIGN_MODULE_EXTENSION: &str = ".dll";

#[cfg(target_os = "macos")]
pub(super) const FOREIGN_MODULE_EXTENSION: &str = ".dylib";

pub(super) fn load_foreign_module(
    path: PathBuf,
    name: &str,
    supermodule: &Rc<RefCell<Module>>,
    line: usize,
) -> compiler::Result<Rc<RefCell<Module>>> {
    // SAFETY: modules must be memory safe
    let library = match unsafe { Library::new(path) } {
        Ok(lib) => Box::leak(Box::new(lib)),
        Err(_) => return Err(load_error(line)),
    };

    let load_function: Symbol<unsafe extern "C" fn() -> ForeignFunctionArray> =
        // SAFETY: the `load_module` symbol must have this type
        match unsafe { library.get(b"load_module\0") } {
            Ok(f) => f,
            Err(_) => return Err(load_error(line)),
        };

    // SAFETY: modules must be memory safe
    let functions = unsafe { load_function() };
    let functions_ptr = functions.functions;
    let num_functions = functions.number;
    let mut i = 0;
    let mut functions: Vec<(Box<dyn EridaniFunction>, String)> = vec![];
    while i < num_functions {
        // SAFETY: the pointer returned from `load_module` must point to
        // a single allocation
        let ptr = unsafe { functions_ptr.offset(i as isize) };
        // SAFETY: the pointer returned from `load_module` must be valid
        let named_function = unsafe { *ptr };

        let foreign_function = named_function.function;
        // SAFETY: `NamedFunction.name` must be a valid string
        let name = unsafe { CStr::from_ptr(named_function.name) };

        let function = move |args: &[Value]| -> Result<Value, ArgumentError> {
            // SAFETY: registered functions must be memory safe
            let returned = unsafe {
                foreign_function(args.len() as i32, args as *const [Value] as *mut c_void)
            };
            // SAFETY: this is the only pointer to this object
            let result = unsafe { Box::from_raw(returned as *mut Result<Value, ArgumentError>) };
            *result
        };

        functions.push((Box::new(function), name.to_string_lossy().to_string()));

        i += 1;
    }

    let mut module = Module::new(name, BTreeMap::new(), None, Some(supermodule));

    module.functions = functions
        .into_iter()
        .map(|(func, name)| Rc::new(RefCell::new(Function::Rust { name, func })))
        .collect();

    module.library = Some(NonNull::from(library));

    Ok(Rc::new(RefCell::new(module)))
}
