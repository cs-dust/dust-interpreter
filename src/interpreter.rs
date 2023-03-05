use std::collections::HashMap;

use crate::interpreter::heap::Heap;
use crate::parser;
use crate::parser::ast::{Expr, Stmt};

mod heap;

pub fn run(ast: &mut Vec<parser::ast::Stmt>) {
    // Take an expression from our parsed program and decide how to handle it.
    fn expression_handler(expr: Expr) -> () {
        match expr {
            Expr::IdentifierExpr(name, source_location) => {}
            Expr::LiteralExpr(literal_value, source_location) => {}
            Expr::BlockExpr(block, source_location) => {}
            Expr::PrimitiveOperationExpr(primitive_op, source_location) => {}
            Expr::AssignmentExpr { assignee, value, position } => {}
            Expr::ApplicationExpr { is_primitive, callee, arguments, position } => {}
            Expr::ReturnExpr(expression, source_location) => {}
        }
    }

    // To be used later, commented to suppress warnings
    // The heap for the program being run by our interpreter
    // let heap = Heap::new();

    // A list of functions in the interpreted program
    // The vector stores all the functions as Stmt values
    // The hashmap allows us to find a function by a string label
    let mut functions_map: HashMap<String, usize> = HashMap::new();
    let mut functions_list: Vec<Stmt> = Vec::new();
    let mut functions_idx: usize = 0;

    // Similar to above, but for statics
    let mut statics_map: HashMap<String, usize> = HashMap::new();
    let mut statics_list: Vec<Stmt> = Vec::new();
    let mut statics_idx: usize = 0;

    // Pop a statement from our ast
    let mut curr_stmt_option: Option<Stmt> = ast.pop();

    // While we have a statement to evaluate
    while (curr_stmt_option.is_some()) {
        let curr: Stmt = curr_stmt_option.expect("How is this even possible bruh"); // current statement
        // add a static or a function to the above lists as necessary.
        // any other type of statement is not allowed in the top level.
        match curr.clone() {
            Stmt::StaticStmt { name, is_mutable, annotation, value, position } => {
                statics_list.push(curr);
                // check if there is a name
                let static_name: String = match name {
                    // there is a name
                    Expr::IdentifierExpr(name_string, sloc) => {
                        // assign that name to static_name
                        name_string
                    }
                    // there is no name
                    _ => {
                        // how is this even possible
                        panic!("no name??");
                    }
                };
                statics_map.insert(static_name, statics_idx);
                statics_idx += 1;
            }
            Stmt::FuncDeclaration { name, lifetime_parameters, parameters, return_type, body, position } => {
                functions_list.push(curr);
                // check if there is a name
                let function_name: String = match name {
                    // there is a name
                    Expr::IdentifierExpr(name_string, sloc) => {
                        // assign that name to function_name
                        name_string
                    }
                    // there is no name
                    _ => {
                        // how is this even possible
                        panic!("no name??");
                    }
                };
                functions_map.insert(function_name, functions_idx);
                functions_idx += 1;
            }
            _ => {
                panic!("Only Function Declarations or Static Statements allowed in top level. ");
            }
        }
        curr_stmt_option = ast.pop();
    }
    // main function is called "main", we use this index to know where to start execution
    let main_idx: usize = functions_map.get("main").expect("No main found!").clone();

    println!("{} function(s) have been declared", functions_idx);
    println!("main function index: {}", main_idx);

    // A is our stack of instructions as in the Source EC evaluator
    let mut A: Vec<Stmt> = Vec::new();
    // We start with main
    A.push(functions_list[main_idx].clone());
    while (!A.is_empty()) {
        let curr: Stmt = A.pop().expect("Literally impossible but ok");
        // see what to do with the statement based on its type
        match curr {
            Stmt::LetStmt { name, is_mutable, annotation, value, position } => {}
            Stmt::StaticStmt { name, is_mutable, annotation, value, position } => {}
            Stmt::FuncDeclaration { name, lifetime_parameters, parameters, return_type, body, position } => {}
            Stmt::ExprStmt(expr) => {
                expression_handler(expr);
            }
        }
    }
}