use super::Program;

use core::fmt;

use serde::{
    de::{self, Visitor},
    ser::SerializeSeq as _,
    Deserialize, Serialize,
};

impl Serialize for Program {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        let mut s = serializer.serialize_seq(Some(3))?;
        s.serialize_element(&self.functions)?;
        s.serialize_element(&self.entry_point)?;
        s.serialize_element(&self.features)?;
        s.end()
    }
}

struct ProgramVisitor;

impl<'de> Visitor<'de> for ProgramVisitor {
    type Value = Program;

    fn expecting(&self, formatter: &mut fmt::Formatter) -> fmt::Result {
        formatter.write_str("a sequence of Vec<Function>, u16, and TargetFeatures")
    }

    fn visit_seq<A>(self, mut seq: A) -> Result<Self::Value, A::Error>
    where
        A: de::SeqAccess<'de>,
    {
        let functions = seq.next_element()?.ok_or(de::Error::invalid_length(
            0,
            &"Expected a sequence of length 3",
        ))?;
        let entry_point = seq.next_element()?.ok_or(de::Error::invalid_length(
            1,
            &"Expected a sequence of length 3",
        ))?;
        let features = seq.next_element()?.ok_or(de::Error::invalid_length(
            2,
            &"Expected a sequence of length 3",
        ))?;
        Ok(Program {
            functions,
            natives: vec![],
            entry_point,
            features,
        })
    }
}
