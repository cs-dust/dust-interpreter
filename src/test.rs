//#![feature(core_panic)]
//use core::panicking::panic;
use std::panic::panic_any;
// use std::cell::RefCell;
use std::collections::HashMap;
// use crate::interpreter::heap::Heap;
use crate::parser;
use crate::parser::ast::{DataType, Expr, PrimitiveOperation, SequenceStmt, Stmt, Block, Literal, UnaryOperator, BinaryOperator, VariadicOperator, PrimitiveOperator};
use crate::parser::ast::Literal::{BoolLiteral, IntLiteral, StringLiteral, UnitLiteral};
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
    Expr(Expr),
    PrimitiveOperation(PrimitiveOperation)
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
        println!("{:#?}", curr);
    }
}

/***************************************************************************************************
* Get the identifier name
***************************************************************************************************/
fn get_name(expr: &Expr) -> String {
    match expr {
        Expr::IdentifierExpr(name, sourcelocation) => name.clone(),
        _ => panic!("fn. get_name could not find identifier to get name from")
    }
}
/***************************************************************************************************
* Stack
***************************************************************************************************/
trait Stack {
    fn peek(&mut self) -> Option<&Literal>;
}

impl Stack for Vec<Literal> {
    fn peek(&mut self) -> Option<&Literal> {
        match self.len() {
            0 => None,
            n => Some(&self[n - 1]),
        }
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

            AgendaInstrs::Block(blk) => blk.evaluate(instr_stack, stash),
            AgendaInstrs::Literal(lit) => lit.evaluate(instr_stack, stash),
            AgendaInstrs::Expr(expr) => expr.evaluate(instr_stack, stash),
            AgendaInstrs::Instructions(instr) => {
                match instr {
                    //     Instructions::Reset => {},
                        Instructions::Assignment(assn) => {
                            let a = Assignment_i {
                                sym: assn.clone().sym
                            };
                            instr_stack.push(AgendaInstrs::Instructions(Instructions::Assignment_i(a)));
                            instr_stack.push(AgendaInstrs::Expr(assn.clone().expr));
                        },
                    //     Instructions::UnOp(unop) => {},
                    //     Instructions::BinOp(binop) => {},
                    Instructions::Pop => {
                        stash.pop();
                        println!("Pop!!")
                    },
                    //     Instructions::App => {},
                    //     Instructions::Branch => {},
                    //     Instructions::Env => {},
                        Instructions::Assignment_i(assn) => {
                            // Peek top of stash
                            let v = match stash.peek() {
                                Some(value) => value.clone(),
                                None => panic!("Why is nothing in the stash??")
                            };
                            // Assign the value to the name in the environment TODO
                            println!("{:#?}", v);
                        }
                    _ => println!("Nothing is here")
                }
            }
            _ => println!("Nothing is here")
        }
    }
}


impl Evaluate for Stmt {
    fn evaluate(&self, instr_stack: &mut Vec<AgendaInstrs>, stash: &mut Vec<Literal>) {
        match self {
            Stmt::LetStmt {name, is_mutable, annotation, value, position} => match value {
                Some(expr) => {
                    let name = get_name(name);
                    // TODO: Put name in the environment
                    instr_stack.push(AgendaInstrs::Literal(Literal::UnitLiteral));
                    instr_stack.push(AgendaInstrs::Instructions(Instructions::Pop));
                    let a = Assignment {
                        sym: name.clone(),
                        expr: expr.clone()
                    };
                    instr_stack.push(AgendaInstrs::Instructions(Instructions::Assignment(a)));
                },
                None => panic!("Unbounded declaration is currently unsupported!")
            },
            Stmt::FuncDeclaration { name, lifetime_parameters, parameters, return_type, body, position } => {
                // let no_params = parameters.len();
                // parameters
                //     .iter()
                //     .map(|(expr, _) | get_name(expr))
                //     .collect::<Vec<String>>()
                //     .into_iter()
                //     .for_each(|name| {
                //         // Add each variable to the env
                //     });
                // TODO: Fix
                instr_stack.push(AgendaInstrs::Block(body.clone()));
            },
            Stmt::ExprStmt(expr) => match expr {
                Expr::ReturnExpr(expr, loc) => {
                    println!("In the return stmt");
                    instr_stack.push(AgendaInstrs::Instructions(Instructions::Reset));
                    let expr_clone = expr.clone();
                    instr_stack.push(AgendaInstrs::Expr(*expr_clone));
                },
                _ => {
                    instr_stack.push(AgendaInstrs::Literal(Literal::UnitLiteral));
                    instr_stack.push(AgendaInstrs::Instructions(Instructions::Pop));
                    instr_stack.push(AgendaInstrs::Expr((expr.clone())));
                }
            },
            _ => {
                println!("Not a function");
            }
        }
    }
}

impl Evaluate for Block {
    fn evaluate(&self, instr_stack: &mut Vec<AgendaInstrs>, stash: &mut Vec<Literal>) {
        //let locals = scan_out_block_declarations(self);
        // TODO: Locals into the environment, as unassigned
        // locals
        //     .into_iter()
        //     .for_each(|name| {
        //         // TODO: Push into env
        //     });
        // TODO: Push current environment onto agenda
        // TODO: Push each statement onto the agenda (reverse order),similar to handle_sequence in ec-evaluator
        let mut statements_clone = self.statements.clone();
        let mut curr_stmt: Option<SequenceStmt> = statements_clone.pop();
        while curr_stmt.is_some() {
            let curr: SequenceStmt = curr_stmt.expect("No block statements?"); // current statement
            match curr.clone() {
                SequenceStmt::Stmt(s) => {
                    instr_stack.push(AgendaInstrs::Stmt(s));
                    instr_stack.push(AgendaInstrs::Instructions(Instructions::Pop));
                }
                SequenceStmt::Block(b) => instr_stack.push(AgendaInstrs::Block(b))
            }
            curr_stmt = statements_clone.pop();
        }
        instr_stack.pop(); // Remove the last pop that was put
    }
}

impl Evaluate for Expr {
    fn evaluate(&self, instr_stack: &mut Vec<AgendaInstrs>, stash: &mut Vec<Literal>) {
        match self {
            Expr::IdentifierExpr(name, source_location) => {
                // Find the identifier in the environment
                // Push the identifier onto the stash. (Placeholder for now)
                stash.push(Literal::IntLiteral(0)); // TODO: Complete this.
            }
            Expr::LiteralExpr(literal_value, source_location) => {
                literal_value.evaluate(instr_stack, stash);
            }
            Expr::BlockExpr(block, source_location) => {
                block.evaluate(instr_stack, stash); // TODO: DROP MARK ON THE AGENDA
            }
            Expr::PrimitiveOperationExpr(primitive_op, source_location) => {
                let prim_op = **primitive_op;
            }
            Expr::AssignmentExpr { assignee, value, position } => {}
            Expr::ApplicationExpr { is_primitive, callee, arguments, position } => {}
            Expr::ReturnExpr(expression, source_location) => {}
        }
    }
}

impl Evaluate for Literal {
    fn evaluate(&self, instr_stack: &mut Vec<AgendaInstrs>, stash: &mut Vec<Literal>) {
        match self {
            IntLiteral(n) => stash.push(IntLiteral(*n)), // Need to  extract the literal when using it
            BoolLiteral(b) => stash.push(BoolLiteral(*b)),
            StringLiteral(s) => {}, // Heap
            UnitLiteral => { stash.push(UnitLiteral) },
            _ => panic!("This literal type is not supported!")
        }
    }
}