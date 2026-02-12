use crate::{value::Value, ArgumentError};

use core::{cmp::Ordering, hash};

use alloc::{format, string::String};

fn get(args: &[Value], index: usize) -> Result<Value, ArgumentError> {
    args.get(index).map_or_else(
        || {
            let description = format!("No item at index '{index}'");
            Err(ArgumentError::new(&description))
        },
        |value| Ok(value.clone()),
    )
}

fn get_string(args: &[Value], index: usize) -> Result<String, ArgumentError> {
    let value = get(args, index)?;
    if let Value::String(s) = value {
        Ok(s)
    } else {
        let description = format!("Expected a string, found '{value}'");
        Err(ArgumentError::new(&description))
    }
}

mod basic {
    use crate::{natives::get, value::Value, ArgumentError};

    use alloc::format;

    pub(crate) fn index(args: &[Value]) -> Result<Value, ArgumentError> {
        let list = get(args, 0)?;
        let Value::List(list) = list else {
            return Err(ArgumentError::new("Expect list"));
        };

        let index = get(args, 1)?;
        let Value::Number(index) = index else {
            return Err(ArgumentError::new("Expect index"));
        };
        let index = index.floor() as usize;

        Ok(list.get(index).map_or(Value::Nothing, Clone::clone))
    }

    pub(crate) fn number(args: &[Value]) -> Result<Value, ArgumentError> {
        let value = get(args, 0)?;
        match &value {
            Value::Number(_) => Ok(value),
            // replace with `if let` guard once they are stabilised
            Value::String(s) if s.parse::<f64>().is_ok() => Ok(Value::Number(s.parse().unwrap())),
            _ => {
                let description = format!("Invalid base for number: {value}");
                Err(ArgumentError::new(&description))
            }
        }
    }

    pub(crate) fn string(args: &[Value]) -> Result<Value, ArgumentError> {
        let value = get(args, 0)?;
        Ok(Value::String(format!("{value}")))
    }
}

#[cfg(feature = "std")]
mod feature_std {
    use crate::{
        natives::{get, get_string},
        value::Value,
        ArgumentError,
    };

    use std::{
        io::{self, Write},
        print, println,
        string::String,
    };

    pub(crate) fn print(args: &[Value]) -> Result<Value, ArgumentError> {
        let item = get(args, 0)?;

        #[cfg(debug_assertions)]
        print!("<stdout> ");

        println!("{item}");
        Ok(Value::Nothing)
    }

    pub(crate) fn input(args: &[Value]) -> Result<Value, ArgumentError> {
        let prompt = get_string(args, 0)?;
        print!("{prompt}");

        let _ = io::stdout().flush();

        let mut buf = String::new();
        if io::stdin().read_line(&mut buf).is_ok() {
            let input = buf
                .strip_suffix("\r\n")
                .or_else(|| buf.strip_suffix('\n'))
                .unwrap_or(&buf)
                .into();
            Ok(Value::String(input))
        } else {
            Err(ArgumentError::new("Failed to read from stdin"))
        }
    }
}

#[cfg(not(feature = "std"))]
mod feature_std {
    use crate::{value::Value, ArgumentError};

    pub fn print(args: &[Value]) -> Result<Value, ArgumentError> {
        unimplemented!()
    }

    pub fn input(args: &[Value]) -> Result<Value, ArgumentError> {
        unimplemented!()
    }
}

#[cfg(feature = "web")]
mod feature_web {}

#[cfg(not(feature = "web"))]
mod feature_web {}

type NativeFunctionInner = fn(&[Value]) -> Result<Value, ArgumentError>;

#[derive(Debug, Clone, Copy)]
pub struct NativeFunction {
    pub name: &'static str,
    pub fun: NativeFunctionInner,
}

impl NativeFunction {
    const fn new(name: &'static str, fun: NativeFunctionInner) -> NativeFunction {
        NativeFunction { name, fun }
    }
}

impl PartialEq for NativeFunction {
    fn eq(&self, other: &Self) -> bool {
        self.name == other.name
    }
}

impl Eq for NativeFunction {}

impl PartialOrd for NativeFunction {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for NativeFunction {
    fn cmp(&self, other: &Self) -> Ordering {
        self.name.cmp(other.name)
    }
}

impl hash::Hash for NativeFunction {
    fn hash<H: hash::Hasher>(&self, state: &mut H) {
        self.name.hash(state);
    }
}

pub const NATIVES: [NativeFunction; 5] = [
    NativeFunction::new("print", feature_std::print),
    NativeFunction::new("index", basic::index),
    NativeFunction::new("number", basic::number),
    NativeFunction::new("string", basic::string),
    NativeFunction::new("input", feature_std::input),
];
