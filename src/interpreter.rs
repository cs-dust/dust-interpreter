use std::collections::HashMap;

use crate::interpreter::heap::Heap;
use crate::parser;
use crate::parser::ast::{Expr, Stmt};

mod heap;

pub fn run(ast: &mut Vec<parser::ast::Stmt>) {
    let stash: Vec<u64>;
    let heap = Heap::new();
    let mut functions_map: HashMap<String, u64> = HashMap::new();
    let mut functions_list: Vec<Stmt> = Vec::new();
    let mut functions_idx: u64 = 0;
    let mut main_idx: u64 = 0;

    let mut statics_map: HashMap<String, u64> = HashMap::new();
    let mut statics_list: Vec<Stmt> = Vec::new();
    let mut statics_idx: u64 = 0;

    let mut curr_stmt_option: Option<Stmt> = ast.pop();
    while (curr_stmt_option.is_some()) {
        let curr: Stmt = curr_stmt_option.expect("How is this even possible bruh");
        match curr.clone() {
            Stmt::StaticStmt { name, is_mutable, annotation, value, position } => {
                statics_list.push(curr);
                let static_name: String = match name {
                    Expr::IdentifierExpr(name_string, sloc) => {
                        name_string
                    }
                    _ => {
                        panic!("no name??");
                    }
                };
                statics_map.insert(static_name, statics_idx);
                statics_idx += 1;
            }
            Stmt::FuncDeclaration { name, lifetime_parameters, parameters, return_type, body, position } => {
                functions_list.push(curr);
                let function_name: String = match name {
                    Expr::IdentifierExpr(name_string, sloc) => {
                        name_string
                    }
                    _ => {
                        panic!("no name??");
                    }
                };
                if (function_name == "main") {
                    main_idx = functions_idx
                }
                functions_map.insert(function_name, functions_idx);
                functions_idx += 1;
            }
            _ => {
                panic!("Only Function Declarations or Static Statements allowed in top level. ");
            }
        }
        curr_stmt_option = ast.pop();
    }


    println!("{} function(s) have been declared", functions_idx);
}