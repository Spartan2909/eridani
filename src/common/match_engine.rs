use crate::{
    common::value::{Pattern, Value},
    prelude::*,
};

use alloc::collections::BTreeMap;

pub fn match_args(
    patterns: &[(Option<String>, Pattern)],
    args: &[Value],
) -> Option<BTreeMap<String, Value>> {
    if patterns.len() != args.len() {
        return None;
    }

    let names: Vec<String> = patterns
        .iter()
        .filter_map(|(name, _)| name.clone())
        .collect();

    let mut dependencies = vec![];
    for (_, pattern) in patterns {
        let mut pattern_dependencies = vec![];

        for binding in pattern.bindings() {
            if let Some(position) = names.iter().position(|x| x == &binding) {
                pattern_dependencies.push(position);
            }
        }

        dependencies.push(pattern_dependencies);
    }

    let mut bindings = BTreeMap::new();

    // TODO finish metapatterns

    for (value, (name, pattern)) in args.iter().zip(patterns) {
        pattern.matches(value, &mut bindings)?;

        if let Some(name) = name {
            bindings.insert(name.to_owned(), value.to_owned());
        }
    }

    Some(bindings)
}
