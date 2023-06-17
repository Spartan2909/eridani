use crate::{common::value::Value, compiler::analyser::pattern::Pattern};

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

fn resolve_metapatterns(
    patterns: &[(Option<u16>, Pattern)],
    names: &[u16],
) -> Result<Vec<usize>, ()> {
    let mut dependency_indexes = vec![];
    for (_, pattern) in patterns.iter() {
        let mut pattern_dependencies = vec![];

        let references = pattern.references();
        for reference in references {
            if let Some(position) = names.iter().position(|x| x == &reference) {
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

pub(crate) fn match_args(
    patterns: &[(Option<u16>, Pattern)],
    args: &[Value],
    mut variables: Vec<Value>,
) -> Result<Option<Vec<Value>>, ()> {
    if patterns.len() != args.len() {
        return Ok(None);
    }

    let names: Vec<u16> = patterns.iter().filter_map(|(name, _)| *name).collect();

    let order = resolve_metapatterns(patterns, &names)?;

    let mut args: Vec<_> = args.iter().zip(patterns).collect();
    for index in order {
        let (value, (name, pattern)) = &mut args[index];
        variables = if let Some(variables) = pattern.matches(value, variables) {
            variables
        } else {
            return Ok(None);
        };

        if name.is_some() {
            variables.push(value.to_owned());
        }
    }

    Ok(Some(variables))
}
