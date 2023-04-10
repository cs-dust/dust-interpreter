extern crate core;

use std::fs;
use std::path;
use std::process;
use std::env;

mod parser;
mod interpreter;

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
        interpreter::run(&mut ast);
    }
}

#[test]
fn test_string_move() {
    let source = fs::read_to_string("examples/string_move_example.rs").expect("Unable to read file");
    println!("Parsing...\n");
    let mut ast = parser::parse(&source).expect("Failed to parse given program");
    interpreter::run(&mut ast);
}

#[test]
fn test_string() {
    let source = fs::read_to_string("examples/string_example.rs").expect("Unable to read file");
    println!("Parsing...\n");
    let mut ast = parser::parse(&source).expect("Failed to parse given program");
    interpreter::run(&mut ast);
}

#[test]
fn test_hello_world() {
    let source = fs::read_to_string("examples/hello_world.rs").expect("Unable to read file");
    println!("Parsing...\n");
    let mut ast = parser::parse(&source).expect("Failed to parse given program");
    interpreter::run(&mut ast);
}

#[test]
fn test_string_concat_move1() {
    let source = fs::read_to_string("examples/string_concat_move_example1.rs").expect("Unable to read file");
    println!("Parsing...\n");
    let mut ast = parser::parse(&source).expect("Failed to parse given program");
    interpreter::run(&mut ast);
}

#[test]
fn test_string_concat_move2() {
    let source = fs::read_to_string("examples/string_concat_move_example2.rs").expect("Unable to read file");
    println!("Parsing...\n");
    let mut ast = parser::parse(&source).expect("Failed to parse given program");
    interpreter::run(&mut ast);
}