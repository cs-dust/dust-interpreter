use core::panicking::panic;
// use std::panic::panic_any;
// use std::cell::RefCell;
use std::collections::HashMap;
// use crate::interpreter::heap::Heap;
use crate::parser;
use crate::parser::ast::{DataType, Expr, PrimitiveOperation, SequenceStmt, Stmt, Block, Literal, UnaryOperator, BinaryOperator, VariadicOperator, PrimitiveOperator};
use crate::parser::ast::Literal::{BoolLiteral, IntLiteral, StringLiteral, UnitLiteral};
use std::ops::Deref;

mod heap;

struct TopLevelMap {
    map: HashMap<String, usize>,
    list: Vec<Stmt>,
    idx: usize
}

struct UnOp {
    sym: UnaryOperator
}

struct BinOp {
    sym: BinaryOperator
}

pub enum Instructions {
    Reset,
    Assignment,
    UnOp,
    BinOp,
    Pop,
    App,
    Branch,
    Env,
    ArrLit,
    ArrAcc,
    ArrAssignment
}

pub enum AgendaInstrs {
    Instructions,
    Stmt,
    Block,
    SequenceStmt
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
    let mut A: Vec<Stmt> = Vec::new();
    // S is the stash of evaluated values
    let mut S: Vec<Literal> = Vec::new(); // TODO: Environments
    // We start with main
    A.push(functions.list[main_idx].clone());
    while !A.is_empty() {
        let curr: Stmt = A.pop().expect("Literally impossible but ok");
        curr.evaluate(&mut A, &mut S); // Borrowing w/ Mutable Reference
    }
}
/***************************************************************************************************
* Operators
***************************************************************************************************/
pub fn binop_microcode_num(x: i64, y: i64, sym: BinaryOperator) -> crate::interpreter::Literal {
    let output =  match sym {
        BinaryOperator::Plus => x + y,
        BinaryOperator::Minus => x - y,
        BinaryOperator::Times => x * y,
        BinaryOperator::Divide => x / y,
        _ => panic!("fn. binop_microcode_num operation unsupported!")
    };
    return IntLiteral(output);
}

pub fn binop_microcode_num_bool(x: i64, y: i64, sym: BinaryOperator) -> crate::interpreter::Literal {
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

pub fn binop_microcode_bool(x: bool, y: bool, sym: BinaryOperator) -> crate::interpreter::Literal {
    let output =  match sym {
        BinaryOperator::And => x && y,
        BinaryOperator::Or => x || y,
        _ => panic!("fn. binop_microcode_bool operation unsupported!")
    };
    return BoolLiteral(output);
}

// Need to check if both x and y have matching types - FIXED
pub fn apply_binop(x: Literal, y: Literal, sym: BinaryOperator) -> Literal {
    let output = match x { // Check type of x
      Literal::IntLiteral(x_num) => match y {
          Literal::IntLiteral(y_num) => match sym { // Ensure x & y are both integer types
              BinaryOperator::Plus|BinaryOperator::Minus|BinaryOperator::Divide|BinaryOperator::Times => binop_microcode_num(x_num, y_num, sym),
              BinaryOperator::Equal|BinaryOperator::NotEqual|BinaryOperator::Greater|BinaryOperator::GreaterOrEqual|BinaryOperator::Less|BinaryOperator::LessOrEqual => binop_microcode_num_bool(x_num, y_num, sym),
              _ => panic!("fn. apply_binop operator unsupported for types x(int) and y(int)!")
          }
          _ => panic!("fn. apply_binop x(int) and y have different types!")
      }
      Literal::BoolLiteral(x_bool) => match y {
          Literal::BoolLiteral(y_bool) => match sym { // Ensure x any y are both bool types
              BinaryOperator::And|BinaryOperator::Or => binop_microcode_bool(x_bool, y_bool, sym),
              _ => panic!("fn. apply_binop operator unsupported for types x(bool) and y(bool)!")
          }
          _ => panic!("fn. apply_binop x(bool) and y have different types!")
      }
      _=> panic!("fn. apply_binop primitive operations unsupported for this type!")
    };
    return output;
}

pub fn unop_microcode_bool(x: bool, sym: UnaryOperator) -> crate::interpreter::Literal {
    let output = match sym {
        UnaryOperator::Not => !x,
        _=> panic!("fn. unop_microcode_bool unsupported operator!")
    };
    return BoolLiteral(output);
}

pub fn unop_microcode_num(x: i64, sym: UnaryOperator) -> crate::interpreter::Literal {
    let output = match sym {
        UnaryOperator::UnaryMinus => -x,
        _=> panic!("fn. unop_microcode_num unsupported operator!")
    };
    return IntLiteral(output);
}

pub fn apply_unop(x: Literal, sym: UnaryOperator) -> Literal {
    let output = match x {
        Literal::IntLiteral(value) => unop_microcode_num(value, sym),
        Literal::BoolLiteral(value) => unop_microcode_bool(value, sym),
        _ => panic!("fn. apply_binop unsupported type for x!")
    };
    return output;
}

/***************************************************************************************************
* Blocks
***************************************************************************************************/
fn scan_declaration_names_from_block(block: &Block) -> Vec<String> { // Fix
    let stmts_in_block: Vec<Stmt> = block.statements
        .iter()
        .fold(vec![], |mut stmts, seq_stmt| match seq_stmt {
            SequenceStmt::Stmt(stmt) => {
                stmts.push(stmt.clone());
                stmts
            },
            _ => stmts,
        });

    scan_declaration_names(&stmts_in_block)
}

/***************************************************************************************************
* Evaluation
***************************************************************************************************/
pub trait Evaluate {
    fn evaluate(&self, instr_stack: &mut Vec<Stmt>, stash: &mut Vec<Literal>);
}

impl Evaluate for Stmt {
    fn evaluate(&self, instr_stack: &mut Vec<Stmt>, stash: &mut Vec<Literal>) {
        match self {
            Stmt::LetStmt { name, is_mutable, annotation, value, position } => {}
            Stmt::StaticStmt { name, is_mutable, annotation, value, position } => {}
            Stmt::FuncDeclaration { name, lifetime_parameters, parameters, return_type, body, position } => {}
            Stmt::ExprStmt(expr) => expr.evaluate(instr_stack, stash)
        }
    }
}


impl Evaluate for Expr {
    fn evaluate(&self, instr_stack: &mut Vec<Stmt>, stash: &mut Vec<Literal>) {
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
                block.evaluate(instr_stack, stash);
            }
            Expr::PrimitiveOperationExpr(primitive_op, source_location) => {}
            Expr::AssignmentExpr { assignee, value, position } => {}
            Expr::ApplicationExpr { is_primitive, callee, arguments, position } => {}
            Expr::ReturnExpr(expression, source_location) => {}
        }
    }
}

impl Evaluate for SequenceStmt {
    fn evaluate(&self, instr_stack: &mut Vec<Stmt>, stash: &mut Vec<Literal>) {
        match self {
            SequenceStmt::Stmt(stmt) => stmt.evaluate(instr_stack, stash),
            SequenceStmt::Block(block) => block.evaluate(instr_stack, stash),
        }
    }
}

impl Evaluate for Block {
    fn evaluate(&self, instr_stack: &mut Vec<Stmt>, stash: &mut Vec<Literal>) {
        let locals =
        for stmt in &self.statements {
            stmt.evaluate(instr_stack, stash);
        }
    }
}

impl Evaluate for PrimitiveOperation{
    fn evaluate(&self, instr_stack: &mut Vec<Stmt>, stash: &mut Vec<Literal>) {
        match self {
            PrimitiveOperation::UnaryOperation { operator, operand } => {
                operand.evaluate(instr_stack, stash);
                operator.evaluate(instr_stack, stash);
            },
            PrimitiveOperation::BinaryOperation { operator, first_operand, second_operand } => {
                first_operand.evaluate(instr_stack, stash);
                second_operand.evaluate(instr_stack, stash);
                operator.evaluate(instr_stack, stash);
            },
            PrimitiveOperation::VariadicOperation { operator, operands } => {
                for operand in operands {
                    operand.evaluate(instr_stack, stash);
                }
                operator.evaluate(instr_stack, stash);
            },
        }
    }
}


impl Evaluate for UnaryOperator {
    fn evaluate(&self, instr_stack: &mut Vec<Stmt>, stash: &mut Vec<Literal>) {
        let operand = stash.pop().expect("Not enough operands on stack");
        let result = match operand {
            IntLiteral(x) => unop_microcode_num(x, *self),
            BoolLiteral(value) => unop_microcode_bool(value, *self),
            _ => panic!("UnaryOperator: Unsupported unary operations"),
        };
        stash.push(result);
    }
}

impl Evaluate for BinaryOperator {
    fn evaluate(&self, instr_stack: &mut Vec<Stmt>, stash: &mut Vec<Literal>) {
        let rhs = stash.pop().expect("Not enough operands on stack");
        let lhs = stash.pop().expect("Not enough operands on stack");
        let result = match (lhs, rhs) {
            (IntLiteral(x), IntLiteral(y)) => {
                binop_microcode_num(x, y, *self)
            }
            (BoolLiteral(x), BoolLiteral(y)) => {
                binop_microcode_bool(x, y, *self)
            }
            _ => panic!("BinaryOperator: Unsupported binary operation"),
        };
        stash.push(result);
    }
}

impl Evaluate for VariadicOperator {
    fn evaluate(&self, instr_stack: &mut Vec<Stmt>, stash: &mut Vec<Literal>) {
        match self {
            //todo!
            _ => unimplemented!(),
        }
    }
}

impl Evaluate for Literal {
    fn evaluate(&self, instr_stack: &mut Vec<Stmt>, stash: &mut Vec<Literal>) {
        match self {
            IntLiteral(n) => stash.push(IntLiteral(*n)), // Need to  extract the literal when using it
            BoolLiteral(b) => stash.push(BoolLiteral(*b)),
            StringLiteral(s) => {}, // Heap
            UnitLiteral => {}, // Heap
        }
    }
}
