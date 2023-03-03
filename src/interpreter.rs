mod heap;

use crate::interpreter::heap::Heap;
use crate::parser;

pub fn run(ast:&Vec<parser::ast::Stmt>) {
    let heap = Heap::new();
}