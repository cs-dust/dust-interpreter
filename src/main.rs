extern crate core;

use std::env;
use std::fs;
use std::io;
use std::process;

mod interpreter;
mod parser;
mod test;

const DEBUG_MODE: bool = false;

fn main() {
    let args: Vec<String> = env::args().collect();
    let mut path = String::new();
    if args.len() == 1 {
        println!("Enter a file path to open: ");
        let stdin = io::stdin();
        let _ = stdin.read_line(&mut path);
        path = path.trim().to_string();
    } else {
        path = args[1].clone();
    }
    println!("Opening {}", &path);
    let source = fs::read_to_string(path).expect("Unable to open file.");
    println!("Parsing...\n");
    let mut ast = parser::parse(&source).expect("Failed to parse given program");
    if ast.len() < 1 {
        println!(
            "Program has no executable units. To compile your program, please add a function."
        );
        process::exit(0);
    }
    interpreter::run(&mut ast, DEBUG_MODE);
}
