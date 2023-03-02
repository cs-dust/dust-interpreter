mod parser;

use std::fs;
use std::process;

fn main() {
    let source = fs::read_to_string("./examples/expression_example.rs").expect("Unable to read file");
    println!("Parsing...\n");
    let ast = parser::parse(&source).expect("Failed to parse given program");
    if ast.len() < 1 {
        println!("Program has no executable units. To compile your program, please add a function.");
        process::exit(0);
    }
    println!("{:#?}", ast);
}