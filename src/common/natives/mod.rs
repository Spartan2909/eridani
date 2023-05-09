use crate::{
    common::{value::Value, ArgumentError},
    prelude::*,
};

mod basic {
    use crate::{
        common::{get, value::Value, ArgumentError},
        prelude::*,
    };

    pub fn index(args: &[Value]) -> Result<Value, ArgumentError> {
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

        Ok(list.get(index).into())
    }

    pub fn number(args: &[Value]) -> Result<Value, ArgumentError> {
        let value = get(args, 0)?;
        match &value {
            Value::Number(_) => Ok(value),
            Value::String(s) if let Ok(n) = s.parse() => Ok(Value::Number(n)),
            _ => {
                let description = format!("Invalid base for number: {value}");
                Err(ArgumentError::new(&description))
            }
        }
    }

    pub fn string(args: &[Value]) -> Result<Value, ArgumentError> {
        let value = get(args, 0)?;
        Ok(Value::String(format!("{value}")))
    }
}

#[cfg(feature = "std")]
mod feature_std {
    use crate::{
        common::{get, get_string, value::Value, ArgumentError},
        prelude::*,
    };

    use std::io::{self, Write};

    pub fn print(args: &[Value]) -> Result<Value, ArgumentError> {
        let item = get(args, 0)?;

        #[cfg(debug_assertions)]
        print!("<stdout> ");

        println!("{item}");
        Ok(Value::Nothing)
    }

    pub fn input(args: &[Value]) -> Result<Value, ArgumentError> {
        let prompt = get_string(args, 0)?;
        print!("{prompt}");

        let _ = io::stdout().flush();

        let mut buf = String::new();
        match io::stdin().read_line(&mut buf) {
            Ok(_) => {
                let input = buf
                    .strip_suffix("\r\n")
                    .or(buf.strip_suffix("\n"))
                    .unwrap_or(&buf)
                    .to_string();
                Ok(Value::String(input))
            },
            Err(_) => Err(ArgumentError::new("Failed to read from stdin")),
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

pub const NATIVES: [(&str, NativeFunction); 5] = [
    ("print", feature_std::print),
    ("index", basic::index),
    ("number", basic::number),
    ("string", basic::string),
    ("input", feature_std::input)
];
