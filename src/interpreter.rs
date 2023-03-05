use std::collections::HashMap;

use crate::interpreter::heap::Heap;
use crate::parser;
use crate::parser::ast::{Expr, Stmt};

mod heap;

pub fn run(ast: &mut Vec<parser::ast::Stmt>) {
    fn ucode(expr: Expr) -> () {
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

    let stash: Vec<u64>;
    let heap = Heap::new();
    let mut functions_map: HashMap<String, usize> = HashMap::new();
    let mut functions_list: Vec<Stmt> = Vec::new();
    let mut functions_idx: usize = 0;

    let mut statics_map: HashMap<String, usize> = HashMap::new();
    let mut statics_list: Vec<Stmt> = Vec::new();
    let mut statics_idx: usize = 0;

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
    let mut main_idx: usize = functions_map.get("main").expect("No main found!").clone();
    println!("main function index: {}", main_idx);
    let mut A: Vec<Stmt> = Vec::new();
    A.push(functions_list[main_idx].clone());
    while (!A.is_empty()) {
        let curr: Stmt = A.pop().expect("Literally impossible but ok");
        match curr {
            Stmt::LetStmt { name, is_mutable, annotation, value, position } => {}
            Stmt::StaticStmt { name, is_mutable, annotation, value, position } => {}
            Stmt::FuncDeclaration { name, lifetime_parameters, parameters, return_type, body, position } => {}
            Stmt::ExprStmt(expr) => {
                ucode(expr);
            }
        }
    }
}