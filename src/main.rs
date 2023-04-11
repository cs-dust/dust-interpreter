extern crate core;

use std::fs;
use std::path;
use std::process;
use std::env;

mod parser;
mod interpreter;
mod test;

const DEBUG_MODE: bool = false;

fn main() {
    let args: Vec<String> = env::args().collect();
    let paths = fs::read_dir("examples/").unwrap();
    for path in paths {
        let source = fs::read_to_string(path.unwrap().path()).expect("Unable to read file");
        println!("Parsing...\n");
        let mut ast = parser::parse(&source).expect("Failed to parse given program");
        if ast.len() < 1 {
            println!("Program has no executable units. To compile your program, please add a function.");
            process::exit(0);
        }
        //println!("{:#?}", ast);
        interpreter::run(&mut ast, DEBUG_MODE);
    }
}