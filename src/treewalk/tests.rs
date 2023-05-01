use crate::{parse, walk_tree};

extern crate test;
use test::{black_box, Bencher};

#[bench]
fn fibonacci(_b: &mut Bencher) {
    let tree = black_box(parse(include_str!("fib.eri"), None, "main")).unwrap();
    black_box(walk_tree(tree, &[])).unwrap();
}
