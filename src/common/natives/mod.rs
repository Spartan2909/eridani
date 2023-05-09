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
}

#[cfg(feature = "std")]
mod feature_std {
    use crate::{
        common::{get, value::Value, ArgumentError},
        prelude::*,
    };

    pub fn print(args: &[Value]) -> Result<Value, ArgumentError> {
        let item = get(args, 0)?;

        #[cfg(debug_assertions)]
        print!("<stdout> ");

        println!("{}", item);
        Ok(Value::Nothing)
    }
}

#[cfg(not(feature = "std"))]
mod feature_std {
    use crate::common::{value::Value, ArgumentError};

    pub fn print(args: &[Value]) -> Result<Value, ArgumentError> {
        unimplemented!()
    }
}

#[cfg(feature = "web")]
mod feature_web {}

#[cfg(not(feature = "web"))]
mod feature_web {}

type NativeFunction = fn(&[Value]) -> Result<Value, ArgumentError>;

pub const NATIVES: [(&str, NativeFunction); 2] =
    [("print", feature_std::print), ("index", basic::index)];
