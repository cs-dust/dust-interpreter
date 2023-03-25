use std::panic::panic_any;
// use std::cell::RefCell;
use std::collections::HashMap;
//use crate::interpreter::heap::Heap;
use crate::parser;
use crate::parser::ast::{DataType, Expr, PrimitiveOperation, SequenceStmt, Stmt, Block, Literal, UnaryOperator, BinaryOperator, VariadicOperator, PrimitiveOperator};
use crate::parser::ast::Literal::{BoolLiteral, IntLiteral, StringLiteral, UnitLiteral};
use std::ops::Deref;
use std::process::id;
use std::string::String;

#[derive(Debug, Clone)]
pub struct Environment {
    store: HashMap<String, u64>,
    pool: Vec<Literal>
}

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
struct Mark {
    mem: u64
}

#[derive(Debug, Clone)]
pub enum Instructions {
    Reset,
    Mark(Mark),
    Assignment(Assignment),
    UnOp(UnOp),
    BinOp(BinOp),
    Pop,
    App,
    Branch,
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
    let mut S: Vec<Literal> = Vec::new();
    // E is the environment
    let mut E = Environment {
        store: HashMap::new(),
        pool: vec![]
    };
    // We start with main
    A.push(AgendaInstrs::Stmt(functions.list[main_idx].clone()));
    while !A.is_empty() {
        let curr: AgendaInstrs = A.pop().expect("Literally impossible but ok");
        curr.evaluate(&mut A, &mut S, &mut E); // Borrowing w/ Mutable Reference
    }
}

/***************************************************************************************************
* Operators
***************************************************************************************************/
pub fn binop_microcode_num(x: i64, y: i64, sym: BinaryOperator) -> Literal {
    let output =  match sym {
        BinaryOperator::Plus => x + y,
        BinaryOperator::Minus => x - y,
        BinaryOperator::Times => x * y,
        BinaryOperator::Divide => x / y,
        _ => panic!("fn. binop_microcode_num operation unsupported!")
    };
    return IntLiteral(output);
}

pub fn binop_microcode_num_bool(x: i64, y: i64, sym: BinaryOperator) -> Literal {
    let output = match sym {
        BinaryOperator::Equal => x == y,
        BinaryOperator::NotEqual => x != y,
        BinaryOperator::Greater => x > y,
        BinaryOperator::GreaterOrEqual => x >= y,
        BinaryOperator::Less => x < y,
        BinaryOperator::LessOrEqual => x <= y,
        _ => panic!("fn. binop_microcode_num_bool operation unsupported!")
    };
    return BoolLiteral(output);
}

pub fn binop_microcode_bool(x: bool, y: bool, sym: BinaryOperator) -> Literal {
    let output =  match sym {
        BinaryOperator::And => x && y,
        BinaryOperator::Or => x || y,
        _ => panic!("fn. binop_microcode_bool operation unsupported!")
    };
    return BoolLiteral(output);
}

pub fn apply_binop(x: Option<Literal>, y: Option<Literal>, sym: BinaryOperator) -> Literal {
    let output = match x { // Check type of x
        Some(Literal::IntLiteral(x_num)) => match y {
            Some(Literal::IntLiteral(y_num)) => match sym { // Ensure x & y are both integer types
                BinaryOperator::Plus|BinaryOperator::Minus|BinaryOperator::Divide|BinaryOperator::Times => binop_microcode_num(x_num, y_num, sym),
                BinaryOperator::Equal|BinaryOperator::NotEqual|BinaryOperator::Greater|BinaryOperator::GreaterOrEqual|BinaryOperator::Less|BinaryOperator::LessOrEqual => binop_microcode_num_bool(x_num, y_num, sym),
                _ => panic!("fn. apply_binop operator unsupported for types x(int) and y(int)!")
            }
            _ => panic!("fn. apply_binop x(int) and y have different types!")
        }
        Some(Literal::BoolLiteral(x_bool)) => match y {
            Some(Literal::BoolLiteral(y_bool)) => match sym { // Ensure x any y are both bool types
                BinaryOperator::And|BinaryOperator::Or => binop_microcode_bool(x_bool, y_bool, sym),
                _ => panic!("fn. apply_binop operator unsupported for types x(bool) and y(bool)!")
            }
            _ => panic!("fn. apply_binop x(bool) and y have different types!")
        }
        _=> panic!("fn. apply_binop primitive operations unsupported for this type!")
    };
    return output;
}

pub fn unop_microcode_bool(x: bool, sym: UnaryOperator) -> Literal {
    let output = match sym {
        UnaryOperator::Not => !x,
        _=> panic!("fn. unop_microcode_bool unsupported operator!")
    };
    return BoolLiteral(output);
}

pub fn unop_microcode_num(x: i64, sym: UnaryOperator) -> Literal {
    let output = match sym {
        UnaryOperator::UnaryMinus => -x,
        _=> panic!("fn. unop_microcode_num unsupported operator!")
    };
    return IntLiteral(output);
}

pub fn apply_unop(x: Option<Literal>, sym: UnaryOperator) -> Literal {
    let output = match x {
        Some(Literal::IntLiteral(value)) => unop_microcode_num(value, sym),
        Some(Literal::BoolLiteral(value)) => unop_microcode_bool(value, sym),
        _ => panic!("fn. apply_binop unsupported type for x!")
    };
    return output;
}

/***************************************************************************************************
* Scan out declarations: Adapted from oxido-lang and ec-evaluator
***************************************************************************************************/
fn scan_out_declarations(stmts: &Vec<Stmt>) -> Vec<String> {
    let scanned_stmt = |stmt: &Stmt| match stmt {
        Stmt::LetStmt { name, .. } | Stmt::FuncDeclaration { name, .. } => {
            let identifier = get_name(name);
            vec![identifier]
        }
        _ => vec![]
    };

    stmts.iter()
        .map(scanned_stmt)
        .fold(vec![], |accumulator, element| { // fold is used to convert a collection of items into a single set
            let mut identifiers = accumulator;
            identifiers.extend(element);
            identifiers
        })
}
/***************************************************************************************************
* Blocks
* Adapted from oxido-lang https://github.com/cs4215-seville/oxido-lang/blob/main/src/compiler.rs
***************************************************************************************************/
fn scan_out_block_declarations(block: &Block) -> Vec<String> {
    let block_stmts: Vec<Stmt> = block.statements
        .iter()
        .fold(vec![], |mut accumulate_stmts, seq_stmt| {
            match seq_stmt {
                SequenceStmt::Stmt(stmt) => {
                    accumulate_stmts.push(stmt.clone());
                    accumulate_stmts
                },
                _ => accumulate_stmts
            }
        });
    scan_out_declarations(&block_stmts)
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
    fn evaluate(&self, instr_stack: &mut Vec<AgendaInstrs>, stash: &mut Vec<Literal>, env: &mut Environment);
}

impl Evaluate for AgendaInstrs {
    fn evaluate(&self, instr_stack: &mut Vec<AgendaInstrs>, stash: &mut Vec<Literal>, env: &mut Environment) {
        match self {
            AgendaInstrs::Stmt(stmt) => stmt.evaluate(instr_stack, stash, env),
            // AgendaInstrs::SequenceStmt(stmts) => stmts.evaluate(instr_stack, stash),
            AgendaInstrs::PrimitiveOperation(prim_op) => prim_op.evaluate(instr_stack, stash, env),
            AgendaInstrs::Block(blk) => blk.evaluate(instr_stack, stash, env),
            AgendaInstrs::Literal(lit) => lit.evaluate(instr_stack, stash, env),
            AgendaInstrs::Expr(expr) => expr.evaluate(instr_stack, stash, env),
            AgendaInstrs::Instructions(instr) => {
                match instr {
                    Instructions::Reset => {
                        match instr_stack.pop() {
                            Some(AgendaInstrs::Instructions(Instructions::Mark(m))) => {
                                // TODO: pop_environment and mutate the current environment
                            }, // Mark found?
                            _ => instr_stack.push(AgendaInstrs::Instructions(Instructions::Reset)) // Continue loop by pushing reset back onto agenda
                        }
                    },
                    Instructions::Assignment(assn) => {
                        let a = Assignment_i {
                            sym: assn.clone().sym
                        };
                        instr_stack.push(AgendaInstrs::Instructions(Instructions::Assignment_i(a)));
                        instr_stack.push(AgendaInstrs::Expr(assn.clone().expr));
                    },
                    Instructions::UnOp(unop) => {
                        let operand = stash.pop();
                        let operator = unop.sym;
                        let value = apply_unop(operand, operator);
                        stash.push(value);
                    },
                    Instructions::BinOp(binop) => {
                        let lhs_operand = stash.pop();
                        let rhs_operand = stash.pop();
                        let operator = binop.sym;
                        let value = apply_binop(lhs_operand, rhs_operand, operator);
                        println!("{:#?}", value.clone());
                        stash.push(value);
                    },
                    Instructions::Pop => {
                        stash.pop();
                        // println!("Pop!! {:#?}", stash.pop());
                    },
                    Instructions::App => {},
                    Instructions::Branch => {},
                    Instructions::Mark(m) => {
                        // TODO: pop_environment and mutate the current environment
                    },
                    Instructions::Assignment_i(assn) => {
                        let v = match stash.peek() {
                            Some(value) => value.clone(),
                            None => panic!("Why is nothing in the stash??")
                        };
                        // Assign the value to the name in the environment
                        let nam = assn.clone().sym;
                        match env.store.get(&nam) { // Get index of where it is stored in the pool
                            Some(val) => {
                                env.pool.insert(*val as usize, v); // Assign value to variable
                            }
                            None => {
                                panic!("Variable not initialised in the environment!");
                            }
                        }
                    }
                    _ => println!("Nothing is here")
                }
            }
            _ => println!("Nothing is here")
        }
    }
}


impl Evaluate for Stmt {
    fn evaluate(&self, instr_stack: &mut Vec<AgendaInstrs>, stash: &mut Vec<Literal>, env: &mut Environment) {
        match self {
            Stmt::LetStmt {name, is_mutable, annotation, value, position} => match value {
                Some(expr) => {
                    let name = get_name(name);
                    // TODO: Put name in the environment (?)
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
    fn evaluate(&self, instr_stack: &mut Vec<AgendaInstrs>, stash: &mut Vec<Literal>, env: &mut Environment) {
        let old_env = Mark { // TODO: store the previous env on the heap with mark pointing to that mem location
            mem: 0
        };
        instr_stack.push(AgendaInstrs::Instructions(Instructions::Mark(old_env))); // Drop mark on agenda

        let mut locals = scan_out_block_declarations(self);
        let mut curr_local: Option<String> = locals.pop();
        while curr_local.is_some() {
            // Overwrite current env values if there are similarly named variables
            let curr: String = curr_local.expect("No locals");
            match env.store.get(&curr) { // Get index of where it is stored in the pool
                Some(val) => { // If the map points to some memory in the pool already
                    env.pool.insert(*val as usize, Literal::UnitLiteral); // Unassigned for now
                }
                None => { // Else assign a new spot in the pool and then map the identifier to that
                    env.pool.push(Literal::UnitLiteral);
                    env.store.insert(curr, (env.pool.len() - 1) as u64);
                }
            }
            curr_local = locals.pop();
        }

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
    fn evaluate(&self, instr_stack: &mut Vec<AgendaInstrs>, stash: &mut Vec<Literal>, env: &mut Environment) {
        match self {
            Expr::IdentifierExpr(name, source_location) => {
                match env.store.get(name) { // Find identifier in the environment
                    Some(idx) => {
                        let value = match env.pool.get((*idx) as usize) { // Find identifier in pool
                            Some(lit) => {
                                lit.clone()
                            }
                            None => {
                                panic!("evaluate identifier expr: Identifier is not in pool!");
                            }
                        };
                        stash.push(value);
                    }
                    None => {
                        panic!("evaluate identifier expr: Identifier not found in environment!");
                    }
                }
            }
            Expr::LiteralExpr(literal_value, source_location) => {
                literal_value.evaluate(instr_stack, stash, env);
            }
            Expr::BlockExpr(block, source_location) => { // Block expr need to have a return stmt
                block.evaluate(instr_stack, stash, env);
            }
            Expr::PrimitiveOperationExpr(primitive_op, source_location) => {
                let prim_op = *primitive_op.clone();
                instr_stack.push(AgendaInstrs::PrimitiveOperation(prim_op));
            }
            Expr::AssignmentExpr { assignee, value, position } => {}
            Expr::ApplicationExpr { is_primitive, callee, arguments, position } => {
                // Put the arguments on the agenda (backwards)
                // Store the previous environment
                // Find the function in the function list (ast)
                // Create new environment by binding the parameters
                if is_primitive.is_some() {
                    match is_primitive.unwrap() {
                        PrimitiveOperator::Unary(op) => match op {
                            UnaryOperator::ImmutableBorrow => {},
                            UnaryOperator::MutableBorrow => {},
                            UnaryOperator::Dereference => {},
                            UnaryOperator::StringFrom => {},
                            UnaryOperator::Drop => {},
                            UnaryOperator::Len => {},
                            UnaryOperator::AsStr => {},
                            UnaryOperator::PushStr => {},
                            _ => panic!("Evaluate application: Unknown primitive function!"),
                        },
                        PrimitiveOperator::Binary(op) => panic!("Evaluate application: Unknown primitive function!"),
                        PrimitiveOperator::VariadicOperator(vo) => match vo {
                            VariadicOperator::Println => {

                            }
                        }
                    }
                }
            }
            Expr::ReturnExpr(expression, source_location) => {}
        }
    }
}

impl Evaluate for PrimitiveOperation{
    fn evaluate(&self, instr_stack: &mut Vec<AgendaInstrs>, stash: &mut Vec<Literal>, env: &mut Environment) {
        match self {
            PrimitiveOperation::UnaryOperation { operator, operand } => {
                let un_operator = UnOp {
                    sym: operator.clone()
                };
                let un_operand = operand.clone();
                instr_stack.push(AgendaInstrs::Instructions(Instructions::UnOp(un_operator)));
                instr_stack.push(AgendaInstrs::Expr(un_operand));
            },
            PrimitiveOperation::BinaryOperation { operator, first_operand, second_operand } => {
                let bin_operator = BinOp {
                    sym: operator.clone()
                };
                let lhs_operand = first_operand.clone();
                let rhs_operand = second_operand.clone();
                instr_stack.push(AgendaInstrs::Instructions(Instructions::BinOp(bin_operator)));
                instr_stack.push(AgendaInstrs::Expr(rhs_operand));
                instr_stack.push(AgendaInstrs::Expr(lhs_operand));
            },
            PrimitiveOperation::VariadicOperation { operator, operands } => {
                // TODO
            },
        }
    }
}

impl Evaluate for Literal {
    fn evaluate(&self, instr_stack: &mut Vec<AgendaInstrs>, stash: &mut Vec<Literal>, env: &mut Environment) {
        match self {
            IntLiteral(n) => stash.push(IntLiteral(*n)), // Need to  extract the literal when using it
            BoolLiteral(b) => stash.push(BoolLiteral(*b)),
            StringLiteral(s) => {}, // Heap
            UnitLiteral => { stash.push(UnitLiteral) },
            _ => panic!("This literal type is not supported!")
        }
    }
}