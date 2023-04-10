use std::fs;
use std::path;
use std::process;
use std::env;
use crate::parser;
use crate::interpreter;

#[test]
fn test_empty_program() {
    let source = fs::read_to_string("examples/empty_program.rs").expect("Unable to read file");
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
fn test_simple_assignment() {
    let source = fs::read_to_string("examples/simple_assignment.rs").expect("Unable to read file");
    println!("Parsing...\n");
    let mut ast = parser::parse(&source).expect("Failed to parse given program");
    interpreter::run(&mut ast);
}

#[test]
fn test_simple_unop() {
    let source = fs::read_to_string("examples/simple_unop.rs").expect("Unable to read file");
    println!("Parsing...\n");
    let mut ast = parser::parse(&source).expect("Failed to parse given program");
    interpreter::run(&mut ast);
}

#[test]
fn test_simple_binop() {
    let source = fs::read_to_string("examples/simple_binop.rs").expect("Unable to read file");
    println!("Parsing...\n");
    let mut ast = parser::parse(&source).expect("Failed to parse given program");
    interpreter::run(&mut ast);
}

#[test]
fn test_bool_binop() {
    let source = fs::read_to_string("examples/bool_types_with_binop.rs").expect("Unable to read file");
    println!("Parsing...\n");
    let mut ast = parser::parse(&source).expect("Failed to parse given program");
    interpreter::run(&mut ast);
}

#[test]
fn test_numeric_binop() {
    let source = fs::read_to_string("examples/numeric_types_with_binop.rs").expect("Unable to read file");
    println!("Parsing...\n");
    let mut ast = parser::parse(&source).expect("Failed to parse given program");
    interpreter::run(&mut ast);
}

#[test]
fn test_block_expression() {
    let source = fs::read_to_string("examples/block_expression_example.rs").expect("Unable to read file");
    println!("Parsing...\n");
    let mut ast = parser::parse(&source).expect("Failed to parse given program");
    interpreter::run(&mut ast);
}

#[test]
fn test_block_env() {
    let source = fs::read_to_string("examples/block_env_example.rs").expect("Unable to read file");
    println!("Parsing...\n");
    let mut ast = parser::parse(&source).expect("Failed to parse given program");
    interpreter::run(&mut ast);
}

#[test]
fn test_function_return() {
    let source = fs::read_to_string("examples/function_return_example.rs").expect("Unable to read file");
    println!("Parsing...\n");
    let mut ast = parser::parse(&source).expect("Failed to parse given program");
    interpreter::run(&mut ast);
}

#[test]
fn test_function_return_binop() {
    let source = fs::read_to_string("examples/function_return_addition.rs").expect("Unable to read file");
    println!("Parsing...\n");
    let mut ast = parser::parse(&source).expect("Failed to parse given program");
    interpreter::run(&mut ast);
}

#[test]
fn test_if_else_equality() {
    let source = fs::read_to_string("examples/if_else_example.rs").expect("Unable to read file");
    println!("Parsing...\n");
    let mut ast = parser::parse(&source).expect("Failed to parse given program");
    interpreter::run(&mut ast);
}

#[test]
fn test_if_else_conjunction() {
    let source = fs::read_to_string("examples/if_else_conjunction_example.rs").expect("Unable to read file");
    println!("Parsing...\n");
    let mut ast = parser::parse(&source).expect("Failed to parse given program");
    interpreter::run(&mut ast);
}

#[test]
fn test_if_else_disjunction() {
    let source = fs::read_to_string("examples/if_else_disjunction_example.rs").expect("Unable to read file");
    println!("Parsing...\n");
    let mut ast = parser::parse(&source).expect("Failed to parse given program");
    interpreter::run(&mut ast);
}

#[test]
fn test_while_loop() {
    let source = fs::read_to_string("examples/while_loop_example.rs").expect("Unable to read file");
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
fn test_string_move() {
    let source = fs::read_to_string("examples/string_move_example.rs").expect("Unable to read file");
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

#[test]
fn test_string_overwrite() {
    let source = fs::read_to_string("examples/string_overwrite.rs").expect("Unable to read file");
    println!("Parsing...\n");
    let mut ast = parser::parse(&source).expect("Failed to parse given program");
    interpreter::run(&mut ast);
}

#[test]
fn test_ownership_function() {
    let source = fs::read_to_string("examples/ownership_function_example.rs").expect("Unable to read file");
    println!("Parsing...\n");
    let mut ast = parser::parse(&source).expect("Failed to parse given program");
    interpreter::run(&mut ast);
}

#[test]
fn test_nested_function() {
    let source = fs::read_to_string("examples/nested_function.rs").expect("Unable to read file");
    println!("Parsing...\n");
    let mut ast = parser::parse(&source).expect("Failed to parse given program");
    interpreter::run(&mut ast);
}