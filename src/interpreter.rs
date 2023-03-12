use core::panicking::panic;
use std::cell::RefCell;
use std::collections::HashMap;
// use crate::interpreter::heap::Heap;
use crate::parser;
use crate::parser::ast::{DataType, Expr, PrimitiveOperation, SequenceStmt, Stmt, Block, Literal, UnaryOperator, BinaryOperator, VariadicOperator, PrimitiveOperator};
use crate::parser::ast::Literal::{BoolLiteral, IntLiteral, StringLiteral, UnitLiteral};

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
pub fn binop_microcode_num(x: i64, y: i64, sym: BinaryOperator) -> IntLiteral {
    let output =  match sym {
        BinaryOperator::Plus => x + y,
        BinaryOperator::Minus => x - y,
        BinaryOperator::Times => x * y,
        BinaryOperator::Divide => x / y,
        _ => panic!("fn. binop_microcode_num operation unsupported!")
    };
    return IntLiteral(output);
}

pub fn binop_microcode_num_bool(x: i64, y: i64, sym: BinaryOperator) -> BoolLiteral {
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

pub fn binop_microcode_bool(x: bool, y: bool, sym: BinaryOperator) -> BoolLiteral {
    let output =  match sym {
        BinaryOperator::And => x && y,
        BinaryOperator::Or => x || y,
        _ => panic!("fn. binop_microcode_bool operation unsupported!")
    };
    return BoolLiteral(output);
}

// Need to check if both x and y have matching types - TODO! Important
pub fn apply_binop(x: Literal, y: Literal, sym: BinaryOperator) -> Literal {
    let x_value = match x {
        Literal::IntLiteral(value) => *value,
        Literal::BoolLiteral(value) => *value,
        _ => panic!("fn. apply_binop unsupported type for x!")
    };
    let y_value = match y {
        Literal::IntLiteral(value) => *value,
        Literal::BoolLiteral(value) => *value,
        _ => panic!("fn. apply_binop unsupported type for y!")
    };
    let output = match sym {
        BinaryOperator::Plus|BinaryOperator::Minus|BinaryOperator::Divide|BinaryOperator::Times => binop_microcode_num(x_value, y_value, sym),
        BinaryOperator::Equal|BinaryOperator::NotEqual|BinaryOperator::Greater|BinaryOperator::GreaterOrEqual|BinaryOperator::Less|BinaryOperator::LessOrEqual => binop_microcode_num_bool(x_value, y_value, sym),
        BinaryOperator::And|BinaryOperator::Or => binop_microcode_bool(x_value, y_value, sym),
        _ => panic!("fn. apply_binop unsupported type for sym!")
    };
    return output;
}

pub fn unop_microcode_bool(x: bool, sym: UnaryOperator) -> BoolLiteral {
    let output = match sym {
        UnaryOperator::Not => !x,
        _=> panic!("fn. unop_microcode_bool unsupported operator!")
    };
    return BoolLiteral(output);
}

pub fn unop_microcode_num(x: i64, sym: UnaryOperator) -> IntLiteral {
    let output = match sym {
        UnaryOperator::UnaryMinus => -x,
        _=> panic!("fn. unop_microcode_num unsupported operator!")
    };
    return IntLiteral(output);
}

pub fn apply_unop(x: Literal, sym: UnaryOperator) -> Literal {
    let output = match x {
        Literal::IntLiteral(value) => unop_microcode_num(*value, sym),
        Literal::BoolLiteral(value) => unop_microcode_bool(*value, sym),
        _ => panic!("fn. apply_binop unsupported type for x!")
    };
    return output;
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
    fn evaluate(&self, instr_stack: &mut Vec<Stmt>, stash: &mut Vec<Literal>) {
        match self {
            SequenceStmt::Stmt(stmt) => stmt.evaluate(instr_stack, stash),
            SequenceStmt::Block(block) => block.evaluate(instr_stack, stash),
        }
    }
}

impl Evaluate for Block {
    fn evaluate(&self, instr_stack: &mut Vec<Stmt>, stash: &mut Vec<Literal>) {
        for stmt in &self.statements {
            stmt.evaluate(instr_stack, stash);
        }
    }
}

// todo:
impl Evaluate for PrimitiveOperation {
    fn evaluate(&self, instr_stack: &mut Vec<Stmt>, stash: &mut Vec<Literal>) {
        match self {
            PrimitiveOperation::UnaryOperation { operator, operand } => {
                operand.evaluate(instr_stack, stash);
                // operator.evaluate(stash);
            },
            PrimitiveOperation::BinaryOperation { operator, first_operand, second_operand } => {
                first_operand.evaluate(instr_stack, stash);
                second_operand.evaluate(instr_stack, stash);
                // operator.evaluate(stash);
            },
            PrimitiveOperation::VariadicOperation { operator, operands } => {
                for operand in operands {
                    operand.evaluate(instr_stack, stash);
                }
            },
        }
    }
}

impl Evaluate for PrimitiveOperator {
    fn evaluate(&self, instr_stack: &mut Vec<Stmt>, stash: &mut Vec<Literal>) {
        match self {
            PrimitiveOperator::Unary(operator) => operator.evaluate(instr_stack, stash),
            PrimitiveOperator::Binary(operator) => operator.evaluate(instr_stack, stash),
            PrimitiveOperator::VariadicOperator(operator) => operator.evaluate(instr_stack, stash),
        }
    }
}


impl Evaluate for UnaryOperator {
    fn evaluate(&self, instr_stack: &mut Vec<Stmt>, stash: &mut Vec<Literal>) {
        match self {
            UnaryOperator::Not => {
                let instr = UnOp{sym: '!'};

                //todo!
            },
            UnaryOperator::UnaryMinus => {
                let operand = stash.pop().expect("Invalid type for unary minus operator");
                //todo!
            },
            UnaryOperator::ImmutableBorrow => {
                //todo!
            },
            UnaryOperator::MutableBorrow => {
                //todo!
            },
            UnaryOperator::Dereference => {
                //todo!
            },
            UnaryOperator::StringFrom => {
                //todo!
            },
            UnaryOperator::Drop => {
                let _ = stash.pop();
            },
            UnaryOperator::Len => {
                //todo!
            },
            UnaryOperator::AsStr => {
                //todo!
            },
            UnaryOperator::PushStr => {
                //todo!
            },
            _ => unimplemented!(),
        }
    }
}

impl Evaluate for BinaryOperator {
    fn evaluate(&self, instr_stack: &mut Vec<Stmt>, stash: &mut Vec<Literal>) {
        match self {
            BinaryOperator::Plus => {
                let num_b = stash.pop().expect("Operand should be a number");
                let num_a = stash.pop().expect("Operand should be a number");
                //todo!
            },
            BinaryOperator::Minus => {
                let num_b = stash.pop().expect("Operand should be a number");
                let num_a = stash.pop().expect("Operand should be a number");
                //todo!
            },
            BinaryOperator::Times => {
                let num_b = stash.pop().expect("Operand should be a number");
                let num_a = stash.pop().expect("Operand should be a number");
                //todo!
            },
            BinaryOperator::Divide => {
                let num_b = stash.pop().expect("Operand should be a number");
                let num_a = stash.pop().expect("Operand should be a number");
                //todo!
            },
            BinaryOperator::Equal => {
                let num_b = stash.pop().expect("Operand should be a number");
                let num_a = stash.pop().expect("Operand should be a number");
                //todo!
            },
            BinaryOperator::NotEqual => {
                let num_b = stash.pop().expect("Operand should be a number");
                let num_a = stash.pop().expect("Operand should be a number");
                //todo!
            },
            BinaryOperator::Greater => {
                let num_b = stash.pop().expect("Operand should be a number");
                let num_a = stash.pop().expect("Operand should be a number");
                //todo!
            },
            BinaryOperator::GreaterOrEqual => {
                let num_b = stash.pop().expect("Operand should be a number");
                let num_a = stash.pop().expect("Operand should be a number");
                //todo!
            },
            BinaryOperator::Less => {
                let num_b = stash.pop().expect("Operand should be a number");
                let num_a = stash.pop().expect("Operand should be a number");
                //todo!
            },
            BinaryOperator::LessOrEqual => {
                let num_b = stash.pop().expect("Operand should be a number");
                let num_a = stash.pop().expect("Operand should be a number");
                //todo!
            },
            BinaryOperator::And => {
                let num_b = stash.pop().expect("Operand should be a number");
                let num_a = stash.pop().expect("Operand should be a number");
                //todo!
            },
            BinaryOperator::Or => {
                let num_b = stash.pop().expect("Operand should be a number");
                let num_a = stash.pop().expect("Operand should be a number");
                //todo!
            },
            _ => unimplemented!(),
        }
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
            Literal::IntLiteral(n) => stash.push(IntLiteral(*n)), // Need to  extract the literal when using it
            Literal::BoolLiteral(b) => stash.push(BoolLiteral(*bool)),
            Literal::StringLiteral(s) => {}, // Heap
            Literal::UnitLiteral => {}, // Heap
        }
    }
}
