#[cfg(feature = "tree_walk")]
mod test_with_treewalk {
    use crate::compiler::{
        ir::{analyse_unoptimised, optimise, treewalk::walk_tree},
        parser::parse,
        scanner::scan,
    };

    #[test]
    fn fibonacci() {
        const SOURCE: &str = "
fibonacci is
    (n: 0 | 1) n

    (n: Integer & > 0) do
        fibonacci(n - 1) + fibonacci(n - 2)
    end

main is
    () print(fibonacci(20))
    ";

        let tokens = scan(SOURCE.to_owned()).unwrap();
        let parse_tree = parse(tokens).unwrap();
        let program = analyse_unoptimised(parse_tree, None, "main").unwrap();
        walk_tree(&program, &[]).unwrap();
        let program = optimise(program).unwrap();
        walk_tree(&program, &[]).unwrap();
    }
}
