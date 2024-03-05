#[cfg(feature = "tree_walk")]
mod test_with_treewalk {
    use crate::{
        common::value::Value,
        compiler::{
            arena,
            ir::{analyse_unoptimised, optimise, treewalk::walk_tree},
            parser::parse,
            scanner::scan,
        },
    };

    #[test]
    fn fibonacci() {
        const SOURCE: &str = "
fibonacci is
    (n: 0 | 1) n

    (n: Integer & > 0)
        fibonacci(n - 1) + fibonacci(n - 2)

main is
    () fibonacci(20)
    ";

        let tokens = scan(SOURCE.to_owned()).unwrap();
        let parse_tree = parse(tokens).unwrap();
        let arena = arena::Arena::new();
        let program = analyse_unoptimised(&arena, parse_tree, None, "main").unwrap();
        assert_eq!(walk_tree(&program, &[]).unwrap(), Value::Number(6765.0));
        let program = optimise(program).unwrap();
        assert_eq!(walk_tree(&program, &[]).unwrap(), Value::Number(6765.0));
    }
}
