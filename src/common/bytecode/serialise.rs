use crate::common::{
    bytecode::{Function, Program},
    discriminant::TargetFeatures,
    natives::{get_native_index, NATIVES},
    EridaniFunction,
};

use serde::{Deserialize, Serialize};

#[derive(Debug, Clone, Serialize, Deserialize)]
pub(super) struct SerialiseNatives(Vec<(usize, String)>);

impl SerialiseNatives {
    fn from_natives(value: Vec<(EridaniFunction, String)>) -> Option<Self> {
        let values: Option<Vec<(usize, String)>> = value
            .into_iter()
            .map(|(native, name)| Some((get_native_index(native)?, name)))
            .collect();
        Some(SerialiseNatives(values?))
    }
}

impl From<Vec<(EridaniFunction, String)>> for SerialiseNatives {
    fn from(value: Vec<(EridaniFunction, String)>) -> Self {
        Self::from_natives(value).unwrap()
    }
}

impl TryFrom<SerialiseNatives> for Vec<(EridaniFunction, String)> {
    type Error = &'static str;

    fn try_from(value: SerialiseNatives) -> Result<Self, Self::Error> {
        let natives: Result<Vec<(EridaniFunction, String)>, &'static str> = value
            .0
            .into_iter()
            .map(|(index, name)| Ok((NATIVES.get(index).ok_or("invalid native")?.1, name)))
            .collect();
        Ok(natives?)
    }
}

#[derive(Serialize, Deserialize)]
pub(super) struct SerialiseProgram {
    pub(crate) functions: Vec<Function>,
    pub(crate) natives: SerialiseNatives,
    pub(crate) entry_point: u16,
    pub(crate) features: TargetFeatures,
}

impl From<Program> for SerialiseProgram {
    fn from(value: Program) -> Self {
        SerialiseProgram {
            functions: value.functions,
            natives: value.natives.into(),
            entry_point: value.entry_point,
            features: value.features,
        }
    }
}

impl TryFrom<SerialiseProgram> for Program {
    type Error = &'static str;

    fn try_from(value: SerialiseProgram) -> Result<Self, Self::Error> {
        Ok(Program {
            functions: value.functions,
            natives: value.natives.try_into()?,
            entry_point: value.entry_point,
            features: value.features,
        })
    }
}
