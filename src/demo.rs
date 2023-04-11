use std::fs;
use std::path;
use std::process;
use std::env;
use crate::parser;
use crate::interpreter;

const DEBUG_MODE: bool = false;

/**
 * Demo 1: Primitive values and how they work, show variable declaration, types and printing messages to the console
 **/
#[test]
fn test_primitive_values() {
    let source = fs::read_to_string("examples/primitive_value_example.rs").expect("Unable to read file");
    println!("Parsing...\n");
    let mut ast = parser::parse(&source).expect("Failed to parse given program");
    interpreter::run(&mut ast, DEBUG_MODE);
}

/**
 * Demo 2: Dust’s support for code blocks and how expressions can be evaluated within these blocks, returning the value of the last expression
 **/
#[test]
fn test_block_expression_with_primitive_op() {
    let source = fs::read_to_string("examples/block_expression_primitive_op.rs").expect("Unable to read file");
    println!("Parsing...\n");
    let mut ast = parser::parse(&source).expect("Failed to parse given program");
    interpreter::run(&mut ast, DEBUG_MODE);
}

/**
 * Demo 3: Show loops in combination with other control flow statements like if else
 **/
#[test]
fn test_control_flow_fizzbuzz() {
    let source = fs::read_to_string("examples/control_flow_fizzbuzz.rs").expect("Unable to read file");
    println!("Parsing...\n");
    let mut ast = parser::parse(&source).expect("Failed to parse given program");
    interpreter::run(&mut ast, DEBUG_MODE);
}


/**
 * Demo 4: This program demonstrates how to define and use nested functions in Rust, as well as how to call regular functions
 **/
#[test]
fn test_function_return_nested_vs_normal() {
    let source = fs::read_to_string("examples/function_application_nested_vs_normal.rs").expect("Unable to read file");
    println!("Parsing...\n");
    let mut ast = parser::parse(&source).expect("Failed to parse given program");
    interpreter::run(&mut ast, DEBUG_MODE);
}

/**
 * Demo 5: See how the values on the heap are freed when we go out of scope of the block expression
 **/
#[test]
fn test_block_expression() {
    let source = fs::read_to_string("examples/block_expression_example.rs").expect("Unable to read file");
    println!("Parsing...\n");
    let mut ast = parser::parse(&source).expect("Failed to parse given program");
    interpreter::run(&mut ast, DEBUG_MODE);
}

/**
 * Demo 6: Shows the movability of the owned mutable String type. When a is assigned to b, it’s ownership also transfers to b
 **/
#[test]
fn test_string_move() {
    let source = fs::read_to_string("examples/string_move_example.rs").expect("Unable to read file");
    println!("Parsing...\n");
    let mut ast = parser::parse(&source).expect("Failed to parse given program");
    interpreter::run(&mut ast, DEBUG_MODE);
}

/**
 * Demo 7: Shows how owned mutable strings move when passed to a function but integers don't
 **/
#[test]
fn test_ownership_function() {
    let source = fs::read_to_string("examples/ownership_function_example.rs").expect("Unable to read file");
    println!("Parsing...\n");
    let mut ast = parser::parse(&source).expect("Failed to parse given program");
    interpreter::run(&mut ast, DEBUG_MODE);
}

/**
 * Demo 8: Concatenating 2 owned mutable strings transfers ownership of the left string to the assignee. Right string is (auto) borrowed
 **/
#[test]
fn test_string_concat_move1() {
    let source = fs::read_to_string("examples/string_concat_move_example1.rs").expect("Unable to read file");
    println!("Parsing...\n");
    let mut ast = parser::parse(&source).expect("Failed to parse given program");
    interpreter::run(&mut ast, DEBUG_MODE);
}

/**
 * Demo 9: Concatenating 2 owned mutable strings transfers ownership of the left string to the assignee. Right string here is a immutable,
* it gets pushed and then freed from the heap at the end of operation
 **/
#[test]
fn test_string_concat_move2() {
    let source = fs::read_to_string("examples/string_concat_move_example2.rs").expect("Unable to read file");
    println!("Parsing...\n");
    let mut ast = parser::parse(&source).expect("Failed to parse given program");
    interpreter::run(&mut ast, DEBUG_MODE);
}