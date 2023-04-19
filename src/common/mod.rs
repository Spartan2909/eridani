pub(crate) mod natives;
pub(crate) mod value;
use value::Value;

pub struct ArgumentError {
    description: String,
}

impl ArgumentError {
    pub fn new(description: &str) -> ArgumentError {
        ArgumentError {
            description: description.to_string(),
        }
    }
}

pub trait EridaniFunction: Fn(&[Value]) -> Result<Value, ArgumentError> {
    fn clone_box<'a>(&self) -> Box<dyn 'a + EridaniFunction>
    where
        Self: 'a;
}

impl<F> EridaniFunction for F
where
    F: Fn(&[Value]) -> Result<Value, ArgumentError> + Clone,
{
    fn clone_box<'a>(&self) -> Box<dyn 'a + EridaniFunction>
    where
        Self: 'a,
    {
        Box::new(self.clone())
    }
}

impl<'a> Clone for Box<dyn 'a + EridaniFunction> {
    fn clone(&self) -> Self {
        (**self).clone_box()
    }
}
