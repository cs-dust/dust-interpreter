extern crate core;

use std::fs;
use std::process;

mod parser;
mod interpreter;

fn main() {
    let source = fs::read_to_string("./examples/numeric_types_with_binop.rs").expect("Unable to read file");
    println!("Parsing...\n");
    let mut ast = parser::parse(&source).expect("Failed to parse given program");
    if ast.len() < 1 {
        println!("Program has no executable units. To compile your program, please add a function.");
        process::exit(0);
    }
    //println!("{:#?}", ast);
    interpreter::run(&mut ast);
}