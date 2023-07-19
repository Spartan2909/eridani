use crate::{common::value::Value, compiler::ir::pattern::Pattern};

use core::cell::RefCell;

use alloc::rc::Rc;

#[derive(Debug, Clone)]
struct Dependency {
    pattern_index: usize,
    depends_on_this: Vec<Rc<RefCell<Dependency>>>,
    dependencies: Vec<Rc<RefCell<Dependency>>>,
}

impl PartialEq for Dependency {
    fn eq(&self, other: &Self) -> bool {
        self.pattern_index == other.pattern_index
    }
}

fn is_superset<T: PartialEq>(superset: &[T], subset: &[T]) -> bool {
    for item in subset {
        if !superset.contains(item) {
            return false;
        }
    }

    true
}

pub(super) fn resolve_metapatterns(
    patterns: &[(Option<&String>, Pattern)],
) -> Result<Vec<usize>, ()> {
    let mut dependency_indexes = vec![];
    for (_, pattern) in patterns.iter() {
        let mut pattern_dependencies = vec![];

        let used_names = pattern.names();
        for used_name in used_names {
            if let Some(position) = patterns
                .iter()
                .position(|(name, _)| name == &Some(used_name))
            {
                pattern_dependencies.push(position);
            }
        }

        dependency_indexes.push(pattern_dependencies);
    }

    let mut dependencies: Vec<_> = patterns
        .iter()
        .enumerate()
        .map(|(i, _)| {
            Rc::new(RefCell::new(Dependency {
                pattern_index: i,
                depends_on_this: vec![],
                dependencies: vec![],
            }))
        })
        .collect();
    for (pattern_index, pattern_dependencies) in dependency_indexes.iter().enumerate() {
        for &dependency_index in pattern_dependencies {
            dependencies[pattern_index]
                .borrow_mut()
                .dependencies
                .push(Rc::clone(&dependencies[dependency_index]));
            if dependencies[dependency_index]
                .borrow_mut()
                .dependencies
                .contains(&dependencies[pattern_index])
            {
                return Err(());
            }
            dependencies[dependency_index]
                .borrow_mut()
                .depends_on_this
                .push(Rc::clone(&dependencies[pattern_index]));
        }
    }

    let mut ordered_dependencies = vec![];
    while !dependencies.is_empty() {
        let mut to_move = vec![];

        for (i, dependency) in dependencies.iter().enumerate() {
            if is_superset(&ordered_dependencies, &dependency.borrow().dependencies) {
                to_move.push(i);
            }
        }

        for (moved, &index) in to_move.iter().enumerate() {
            ordered_dependencies.push(dependencies.remove(index - moved));
        }
    }

    let result = ordered_dependencies
        .iter()
        .map(|dep| dep.borrow().pattern_index)
        .collect();

    // Prevent memory leaks
    for dependency in ordered_dependencies {
        dependency.borrow_mut().depends_on_this = vec![];
        dependency.borrow_mut().dependencies = vec![];
    }

    Ok(result)
}

#[derive(Debug, Clone, PartialEq)]
pub(super) enum MatchResult {
    Error,
    Fail,
    Indeterminable,
    Success(Vec<Value>),
}

impl MatchResult {
    pub(super) fn is_ok(&self) -> bool {
        match self {
            MatchResult::Error | MatchResult::Fail => false,
            MatchResult::Indeterminable | MatchResult::Success(_) => true,
        }
    }
}

impl FromIterator<MatchResult> for Option<Vec<Option<Vec<Value>>>> {
    fn from_iter<T: IntoIterator<Item = MatchResult>>(iter: T) -> Self {
        let mut all_values = vec![];

        for result in iter {
            match result {
                MatchResult::Error => return None,
                MatchResult::Success(values) => all_values.push(Some(values)),
                MatchResult::Fail | MatchResult::Indeterminable => all_values.push(None),
            }
        }

        Some(all_values)
    }
}

pub(super) fn partial_match(
    patterns: &[(Option<(u16, bool)>, Pattern)],
    args: &[Option<Value>],
    order: &[usize],
) -> MatchResult {
    if patterns.len() != args.len() {
        return MatchResult::Fail;
    }

    let mut variables = vec![];
    let mut full = true;

    let args: Vec<_> = args.iter().zip(patterns).collect();
    for &index in order {
        let (value, (name, pattern)) = &args[index];
        if let Some(value) = value {
            variables = if let Some(variables) = pattern.matches(value, variables) {
                variables
            } else {
                return MatchResult::Fail;
            };

            if name.is_some() && !name.unwrap().1 {
                variables.push(value.to_owned());
            }
        } else {
            full = false;
        }
    }

    if full {
        MatchResult::Success(variables)
    } else {
        MatchResult::Indeterminable
    }
}

pub(super) fn match_args(
    patterns: &[(Option<(u16, bool)>, Pattern)],
    args: &[Value],
    order: &[usize],
) -> MatchResult {
    let args: Vec<_> = args.iter().map(|value| Some(value.to_owned())).collect();
    partial_match(patterns, &args, order)
}
