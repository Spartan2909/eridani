use crate::{
    common::{value::Value, ArgumentError},
    prelude::*,
};

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
    use crate::{
        common::{natives::get, value::Value, ArgumentError},
        prelude::*,
    };

    pub(crate) fn index(args: &[Value]) -> Result<Value, ArgumentError> {
        let list = get(args, 0)?;
        let list = if let Value::List(list) = list {
            list
        } else {
            return Err(ArgumentError::new("Expect list"));
        };

        let index = get(args, 1)?;
        let index = if let Value::Number(n) = index {
            n
        } else {
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
    use crate::common::{
        natives::{get, get_string},
        value::Value,
        ArgumentError,
    };

    use std::io::{self, Write};

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
                .to_string();
            Ok(Value::String(input))
        } else {
            Err(ArgumentError::new("Failed to read from stdin"))
        }
    }
}

#[cfg(not(feature = "std"))]
mod feature_std {
    use crate::common::{value::Value, ArgumentError};

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

type NativeFunction = fn(&[Value]) -> Result<Value, ArgumentError>;

pub(crate) const NATIVES: [(&str, NativeFunction); 5] = [
    ("print", feature_std::print),
    ("index", basic::index),
    ("number", basic::number),
    ("string", basic::string),
    ("input", feature_std::input),
];

pub(crate) fn get_native_index(f: NativeFunction) -> Option<usize> {
    NATIVES.iter().position(|(_, native)| *native == f)
}
