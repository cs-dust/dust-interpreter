//#![feature(core_panic)]
//use core::panicking::panic;
use std::panic::panic_any;
// use std::cell::RefCell;
use std::collections::HashMap;
// use crate::interpreter::heap::Heap;
use crate::parser;
use crate::parser::ast::{DataType, Expr, PrimitiveOperation, SequenceStmt, Stmt, Block, Literal, UnaryOperator, BinaryOperator, VariadicOperator, PrimitiveOperator};
use crate::parser::ast::Literal::{BoolLiteral, IntLiteral, StringLiteral, UndefinedLiteral, UnitLiteral};
use std::ops::Deref;
use std::process::id;
use std::string::String;

#[derive(Debug, Clone)]
struct TopLevelMap {
    map: HashMap<String, usize>,
    list: Vec<Stmt>,
    idx: usize
}

#[derive(Debug, Clone)]
struct UnOp {
    sym: UnaryOperator
}

#[derive(Debug, Clone)]
struct BinOp {
    sym: BinaryOperator
}

#[derive(Debug, Clone)]
struct Assignment {
    sym: String,
    expr: Expr
}

#[derive(Debug, Clone)]
struct Assignment_i {
    sym: String
}

#[derive(Debug, Clone)]
pub enum Instructions {
    Reset,
    Assignment(Assignment),
    UnOp(UnOp),
    BinOp(BinOp),
    Pop,
    App,
    Branch,
    Env,
    Assignment_i(Assignment_i)
}

#[derive(Debug, Clone)]
pub enum AgendaInstrs {
    Instructions(Instructions),
    Stmt(Stmt),
    Block(Block),
    SequenceStmt(SequenceStmt),
    Literal(Literal),
    Expr(Expr)
}


/***************************************************************************************************
* Run the code
***************************************************************************************************/
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
    while curr_stmt_option.is_some() {
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
    let mut A: Vec<AgendaInstrs> = Vec::new();
    // S is the stash of evaluated values
    let mut S: Vec<Literal> = Vec::new(); // TODO: Environments
    // We start with main
    A.push(AgendaInstrs::Stmt(functions.list[main_idx].clone())); // TODO: Push only main's block onto agenda
    while !A.is_empty() {
        let curr: AgendaInstrs = A.pop().expect("Literally impossible but ok");
        curr.evaluate(&mut A, &mut S); // Borrowing w/ Mutable Reference
        println!("loop");
    }
}

/***************************************************************************************************
* Evaluation
***************************************************************************************************/
pub trait Evaluate {
    fn evaluate(&self, instr_stack: &mut Vec<AgendaInstrs>, stash: &mut Vec<Literal>);
}

impl Evaluate for AgendaInstrs {
    fn evaluate(&self, instr_stack: &mut Vec<AgendaInstrs>, stash: &mut Vec<Literal>) {
        match self {
            AgendaInstrs::Stmt(stmt) => stmt.evaluate(instr_stack, stash),
            // AgendaInstrs::SequenceStmt(stmts) => stmts.evaluate(instr_stack, stash),
            // AgendaInstrs::Block(blk) => blk.evaluate(instr_stack, stash),
            // AgendaInstrs::Literal(lit) => lit.evaluate(instr_stack, stash),
            // AgendaInstrs::Expr(expr) => expr.evaluate(instr_stack, stash),
            // AgendaInstrs::Instructions(instr) => { match instr {
            //     Instructions::Reset => {},
            //     Instructions::Assignment(assn) => {
            //         let a = Assignment_i {
            //             sym: assn.clone().sym
            //         };
            //         instr_stack.push(AgendaInstrs::Instructions(Instructions::Assignment_i(a)));
            //         instr_stack.push(AgendaInstrs::Expr(assn.clone().expr));
            //     },
            //     Instructions::UnOp(unop) => {},
            //     Instructions::BinOp(binop) => {},
                 Instructions::Pop => {
                     
                 },
            //     Instructions::App => {},
            //     Instructions::Branch => {},
            //     Instructions::Env => {},
            //     Instructions::Assignment_i(assn) => {
            //         // Peek top of stash
            //         let v = match stash.peek() {
            //             Some(value) => *(value.clone()),
            //             None => panic!("Why is nothing in the stash??")
            //         };
            //         // Assign the value to the name in the environment TODO
            //     }
            _ => println!("Nothing is here")
            //}
            }
        }
    }


impl Evaluate for Stmt {
    fn evaluate(&self, instr_stack: &mut Vec<AgendaInstrs>, stash: &mut Vec<Literal>) {
        match self {
            Stmt::FuncDeclaration { name, lifetime_parameters, parameters, return_type, body, position } => {
                println!("Function declared");
                instr_stack.push(AgendaInstrs::Instructions(Instructions::Pop));
                stash.push(Literal::IntLiteral(9));
            }
            _ => {
                println!("Not a function");
            }
        }
    }
}