use std::collections::HashMap;

use crate::interpreter::heap::Heap;
use crate::parser;
use crate::parser::ast::{DataType, Expr, PrimitiveOperation, SequenceStmt, Stmt, Block, Literal};

mod heap;

struct TopLevelMap {
    map: HashMap<String, usize>,
    list: Vec<Stmt>,
    idx: usize
}

pub fn run(ast: &mut Vec<parser::ast::Stmt>) {
    // To be used later, commented to suppress warnings
    // The heap for the program being run by our interpreter
    // let heap = Heap::new();

    // A list of functions in the interpreted program
    // The vector stores all the functions as Stmt values
    // The hashmap allows us to find a function by a string label
    let mut functions = TopLevelMap {
        map: HashMap::new(),
        list: Vec::new(),
        idx: 0
    };

    // Similar to above, but for statics
    let mut statics = TopLevelMap {
        map: HashMap::new(),
        list: Vec::new(),
        idx: 0
    };

    // Pop a statement from our ast
    let mut curr_stmt_option: Option<Stmt> = ast.pop();

    // While we have a statement to evaluate
    while (curr_stmt_option.is_some()) {
        let curr: Stmt = curr_stmt_option.expect("How is this even possible bruh"); // current statement
        // add a static or a function to the above lists as necessary.
        // any other type of statement is not allowed in the top level.
        match curr.clone() {
            Stmt::StaticStmt { name, is_mutable, annotation, value, position } => {
                statics.list.push(curr);
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
                statics.map.insert(static_name, statics.idx);
                statics.idx += 1;
            }
            Stmt::FuncDeclaration { name, lifetime_parameters, parameters, return_type, body, position } => {
                functions.list.push(curr);
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
                functions.map.insert(function_name, functions.idx);
                functions.idx += 1;
            }
            _ => {
                panic!("Only Function Declarations or Static Statements allowed in top level. ");
            }
        }
        curr_stmt_option = ast.pop();
    }
    // main function is called "main", we use this index to know where to start execution
    let main_idx: usize = functions.map.get("main").expect("No main found!").clone();

    println!("{} function(s) have been declared", functions.idx);
    println!("main function index: {}", main_idx);

    // A is our stack of instructions as in the Source EC evaluator
    let mut A: Vec<Stmt> = Vec::new();
    // S is the stash of evaluated values
    let mut S: Vec<DataType> = Vec::new(); // TODO: Environments
    // We start with main
    A.push(functions.list[main_idx].clone());
    while (!A.is_empty()) {
        let curr: Stmt = A.pop().expect("Literally impossible but ok");
        curr.evaluate(&mut A, &mut S, &); // Borrowing w/ Mutable Reference
    }
}

pub trait Evaluate {
    fn evaluate(&self, instr_stack: &mut Vec<Stmt>, stash: &mut Vec<DataType>);
}

impl Evaluate for Stmt {
    fn evaluate(&self, instr_stack: &mut Vec<Stmt>, stash: &mut Vec<DataType>) {
        match self {
            Stmt::LetStmt { name, is_mutable, annotation, value, position } => {}
            Stmt::StaticStmt { name, is_mutable, annotation, value, position } => {}
            Stmt::FuncDeclaration { name, lifetime_parameters, parameters, return_type, body, position } => {}
            Stmt::ExprStmt(expr) => expr.evaluate(instr_stack, stash)
        }
    }
}


impl Evaluate for Expr {
    fn evaluate(&self, instr_stack: &mut Vec<Stmt>, stash: &mut Vec<DataType>) {
        match self {
            Expr::IdentifierExpr(name, source_location) => {
                // Find the identifier in the environment
            }
            Expr::LiteralExpr(literal_value, source_location) => {
            }
            Expr::BlockExpr(block, source_location) => {}
            Expr::PrimitiveOperationExpr(primitive_op, source_location) => {}
            Expr::AssignmentExpr { assignee, value, position } => {}
            Expr::ApplicationExpr { is_primitive, callee, arguments, position } => {}
            Expr::ReturnExpr(expression, source_location) => {}
        }
    }
}

impl Evaluate for SequenceStmt {
    fn evaluate(&self, instr_stack: &mut Vec<Stmt>, stash: &mut Vec<DataType>) {
        match self {
            SequenceStmt::Stmt(stmt) => stmt.evaluate(instr_stack, stash),
            SequenceStmt::Block(block) => block.evaluate(instr_stack, stash),
        }
    }
}

impl Evaluate for Block {
    fn evaluate(&self, instr_stack: &mut Vec<Stmt>, stash: &mut Vec<DataType>) {
        //todo!()
    }
}

impl Evaluate for PrimitiveOperation {
    fn evaluate(&self, instr_stack: &mut Vec<Stmt>, stash: &mut Vec<DataType>) {
        //todo!()
    }
}

impl Evaluate for Literal {
    fn evaluate(&self, instr_stack: &mut Vec<Stmt>, stash: &mut Vec<DataType>) {
        //todo!()
    }
}


