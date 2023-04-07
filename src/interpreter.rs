mod environment;
use std::borrow::Borrow;
use environment::Environment;
use environment::Object;
use environment::TopLevelMap;
use std::panic::panic_any;
use std::cell::RefCell;
use std::rc::Rc;
use std::collections::HashMap;
//use crate::interpreter::heap::Heap;
use crate::parser;
use crate::parser::ast::{DataType, Expr, PrimitiveOperation, SequenceStmt, Stmt, Block, Literal, UnaryOperator, BinaryOperator, VariadicOperator, PrimitiveOperator, FuncParameter};
use crate::parser::ast::Literal::{BoolLiteral, IntLiteral, StringLiteral, UnitLiteral};
use std::ops::Deref;
use std::process::id;
use std::string::String;
use crate::parser::ast::Stmt::FuncDeclaration;

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
struct App_i {
    arity: usize,
    builtin: bool,
    sym: String
}

#[derive(Debug, Clone)]
struct Branch_i {
    cons: Expr,
    alt: Option<Expr>,
}

#[derive(Debug, Clone)]
struct Loop_i {
    body: Expr,
}


#[derive(Debug, Clone)]
pub enum Instructions {
    Reset,
    Mark,
    Assignment(Assignment),
    UnOp(UnOp),
    BinOp(BinOp),
    Pop,
    App,
    Branch,
    Assignment_i(Assignment_i),
    App_i(App_i),
    Branch_i(Branch_i),
    Loop_i(Loop_i),
}

#[derive(Debug, Clone)]
pub enum AgendaInstrs {
    Instructions(Instructions),
    Stmt(Stmt),
    Block(Block),
    SequenceStmt(SequenceStmt),
    Literal(Literal),
    Expr(Expr),
    PrimitiveOperation(PrimitiveOperation),
    Environment(Environment)
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
    // The hashmap allows us to find a function by a string label TODO: remove map, not needed anymore
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
        let curr: Stmt = curr_stmt_option.expect("How is this even possible bruh");
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

    let mut A: Vec<AgendaInstrs> = Vec::new(); // A is our stack of instructions as in the Source EC evaluator
    let mut S: Vec<Literal> = Vec::new(); // S is the stash of evaluated values
    let mut global_env = Environment::new();
    global_env.insert_top_level(functions.clone()); // Insert top level declarations into environment
    global_env.insert_top_level(statics.clone());
    let mut E = Box::new(global_env);

    A.push(AgendaInstrs::Stmt(functions.list[main_idx].clone())); // We start with main
    // TODO: Check if we should have a step limit
    while !A.is_empty() {
        let curr: AgendaInstrs = A.pop().expect("Literally impossible but ok");
        curr.evaluate(&mut A, &mut S, &mut E); // Borrowing w/ Mutable Reference
        //println!("{:#?}", curr.clone());
    }
    if S.is_empty() || S.len() > 1 {
        println!("Stash is empty!");
    }
    println!("{:#?}", S[0]);
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
    //print!("{:#?}", x);
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
* Pushing statements onto agenda in reverse
***************************************************************************************************/
pub fn push_block_stmts_reverse(instr_stack: &mut Vec<AgendaInstrs>, statements: Vec<SequenceStmt>) {
    let mut statements_clone = statements.clone();
    let mut curr_stmt = statements_clone.pop();
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

/***************************************************************************************************
* Function parameter extraction
***************************************************************************************************/
pub fn scan_out_param_names(params: &Vec<FuncParameter>) -> Vec<String> {
    let scanned_param = |param: &FuncParameter| -> Vec<String> {
        let (expr, datatype) = param;
        let name = get_name(expr);
        vec![name]
    };

    params.iter()
        .map(scanned_param)
        .fold(vec![], |accumulator, element| {
            let mut identifiers = accumulator;
            identifiers.extend(element);
            identifiers
        })
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
trait Stack<T> {
    fn peek(&mut self) -> Option<&T>;
}

impl Stack<Literal> for Vec<Literal> {
    fn peek(&mut self) -> Option<&Literal> {
        match self.len() {
            0 => None,
            n => Some(&self[n - 1]),
        }
    }
}

impl Stack<AgendaInstrs> for Vec<AgendaInstrs> {
    fn peek(&mut self) -> Option<&AgendaInstrs> {
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
    fn evaluate(&self, instr_stack: &mut Vec<AgendaInstrs>, stash: &mut Vec<Literal>, env: &mut Box<Environment>);
}

impl Evaluate for AgendaInstrs {
    fn evaluate(&self, instr_stack: &mut Vec<AgendaInstrs>, stash: &mut Vec<Literal>, env: &mut Box<Environment>) {
        let mut instr_ptr = instr_stack.len();
        match self {
            AgendaInstrs::Stmt(stmt) => stmt.evaluate(instr_stack, stash, env),
            // AgendaInstrs::SequenceStmt(stmts) => stmts.evaluate(instr_stack, stash),
            AgendaInstrs::PrimitiveOperation(prim_op) => prim_op.evaluate(instr_stack, stash, env),
            AgendaInstrs::Block(blk) => blk.evaluate(instr_stack, stash, env),
            AgendaInstrs::Literal(lit) => lit.evaluate(instr_stack, stash, env),
            AgendaInstrs::Expr(expr) => expr.evaluate(instr_stack, stash, env),
            AgendaInstrs::Environment(ref e) => { // Restore environment
                let mut old_env = e.clone();
                *env = Box::new(old_env); // Restore
            }
            AgendaInstrs::Instructions(instr) => {
                // let mut instr_ptr = 0;
                match instr {
                    Instructions::Reset => {
                        match instr_stack.pop() {
                            Some(AgendaInstrs::Instructions(Instructions::Mark)) => {
                                // Mark found, now environment restoration occurs
                            },
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
                        let rhs_operand = stash.pop();
                        //println!("{:#?}", rhs_operand.clone());
                        let lhs_operand = stash.pop();
                        //println!("{:#?}", lhs_operand.clone());
                        let operator = binop.sym;
                        let value = apply_binop(lhs_operand, rhs_operand, operator);
                        stash.push(value);
                    },
                    Instructions::Pop => {
                        stash.pop();
                        // println!("Pop!! {:#?}", stash.pop());
                    },
                    Instructions::App => {},
                    Instructions::Branch => {},
                    Instructions::Mark => {
                        // Nothing
                    },
                    Instructions::Assignment_i(assn) => {
                        let v = match stash.peek() {
                            Some(value) => value.clone(),
                            None => panic!("Why is nothing in the stash??")
                        };
                        // Assign the value to the name in the environment
                        let nam = assn.clone().sym;
                        env.set(nam, Object::Literal(v));
                    },
                    Instructions::App_i(app) => {
                        let arity= app.arity;
                        let mut args = {
                            if arity == 0 {
                                vec![]
                            } else {
                                let mut arguments = vec![Literal::UnitLiteral; arity];
                                for i in (0..=(arity - 1)).rev() {
                                    arguments[i] = stash.pop().expect("Value should be here if arity is > 0");
                                }
                                arguments
                            }
                        };
                        if app.builtin {
                            match app.sym.as_str() {
                                "println" => println!("{:#?}", args),
                                _ => panic!("Builtin {} not supported!", app.sym.as_str())
                            }
                        } else {
                            let store_env = match instr_stack.peek() {
                                Some(AgendaInstrs::Environment(e)) => true,
                                _ => false
                            };
                            let tail_call = match instr_stack.peek() {
                                Some(AgendaInstrs::Instructions(Instructions::Reset)) => true,
                                _ => false
                            };
                            if instr_stack.len() == 0 || store_env {
                                // Env is not needed, just push mark
                                instr_stack.push(AgendaInstrs::Instructions(Instructions::Mark));
                            } else if tail_call {
                                // Tail call, callee's return will push another reset
                                instr_stack.pop();
                            } else {
                                let old_env = env.clone();
                                instr_stack.push(AgendaInstrs::Environment(*old_env)); // Put environment on agenda
                                instr_stack.push(AgendaInstrs::Instructions(Instructions::Mark));
                            }
                            let func_stmt = match env.get(&*app.sym.clone()) {
                                Some(Object::DeclStatement(s)) => s.clone(),
                                _ => panic!("Function not declared in scope!")
                            };
                            let mut params;
                            let mut fun_body;
                            match func_stmt.clone() {
                                Stmt::FuncDeclaration { name, lifetime_parameters, parameters, return_type, body, position } => {
                                    params = parameters;
                                    fun_body = body;
                                },
                                _ => panic!("Why does the function not have a function declaration?")
                            };
                            // Push the body of the function onto the agenda backwards
                            // Extend the environment with the new vars
                            let mut param_names = scan_out_param_names(&params);
                            let mut locals = scan_out_block_declarations(&fun_body);

                            let outer = env.clone();
                            let mut new_env = Environment::extend_environment(outer);

                            new_env.bind_parameters(param_names, args);
                            new_env.insert_locals(locals);
                            *env = Box::new(new_env); // Change the current env

                            // TODO: Check for function declarations here and add to the environment
                            let mut statements = fun_body.statements.clone();
                            push_block_stmts_reverse(instr_stack, statements);
                        }
                    },
                    Instructions::Branch_i(br) => {
                        let mut cons = br.cons.clone();
                        let mut alt = br.alt.clone();

                        let pred_val = stash.pop().expect("Expected predicate value on stash");

                        // Check value of the predicate and evaluate the consequent or alternative block accordingly
                        match pred_val {
                            Literal::BoolLiteral(b) => {
                                if b {
                                    // Push cons expression to instruction stack
                                    instr_stack.push(AgendaInstrs::Expr(cons));
                                } else {
                                    match alt {
                                        // Push alt expression to instruction stack
                                        Some(alt_expr) => instr_stack.push(AgendaInstrs::Expr(alt_expr)),
                                        // Do nothing if no alt
                                        None => {}
                                    }
                                }
                            },
                            _ => panic!("Predicate type not supported")
                        }
                    }
                    Instructions::Loop_i(lp) => {
                        // let mut body_expr = lp.body.clone();
                        let pred_val = stash.pop().expect("Expected predicate value on stash");

                        // Check value of predicate
                        match pred_val {
                            Literal::BoolLiteral(b) => {
                                if b {
                                    println!("in b");
                                    let mut body_expr = lp.body.clone();
                                    instr_stack.push(AgendaInstrs::Expr(body_expr));
                                    // Push Loop_i instruction back onto the instruction stack to continue the loop
                                    // instr_stack.push(AgendaInstrs::Instructions(Instructions::Loop_i(lp.clone())));
                                    // stash.push(Literal::BoolLiteral(false))
                                // } else {
                                //     instr_stack.pop();
                                }
                            },
                            _=> panic!("Predicate type not supported")
                        }
                    }
                    _ => println!("No instruction?")
                }
            }
            _ => println!("Agenda is empty")
        }
    }
}

impl Evaluate for Stmt {
    fn evaluate(&self, instr_stack: &mut Vec<AgendaInstrs>, stash: &mut Vec<Literal>, env: &mut Box<Environment>) {
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
                instr_stack.push(AgendaInstrs::Block(body.clone()));
            },
            Stmt::ExprStmt(expr) => match expr {
                Expr::ReturnExpr(expr, loc) => {
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
            Stmt::IfElseStmt { pred, cons, alt, position } => {
                // Create new Branch_i instruction with cloned cons and alt expressions
                let mut br = Branch_i {
                    cons: cons.clone(),
                    alt: alt.clone(),
                };
                // Push Branch_i instruction onto instruction stack
                instr_stack.push(AgendaInstrs::Instructions(Instructions::Branch_i(br)));
                // Push predicate expression onto instruction stack
                instr_stack.push(AgendaInstrs::Expr(pred.clone()));
            },
            // Stmt::ForLoopStmt { init, pred, update, body, position } => {
            //
            //     let init = init.clone(); // Clone the init expression
            //     let pred = pred.clone(); // Clone the pred expression
            //     let update = update.clone(); // Clone the update expression
            //     let body = body.clone(); // Clone the body statement
            //
            //     let mut cons = vec![]; // Consequent block
            //
            //     // If init expression is present, push it onto the consequent block
            //     if let Some(init_expr) = init {
            //         cons.push(AgendaInstrs::Expr(init_expr));
            //     }
            //
            //     // Push the update expression, body statement, and pred expression onto the consequent block
            //     cons.push(AgendaInstrs::Expr(update));
            //     cons.push(AgendaInstrs::Stmt(body));
            //     cons.push(AgendaInstrs::Expr(pred));
            //
            //     let mut br = Branch_i {
            //         cons,
            //         alt: None, // No alternative block for for loop
            //     };
            //
            //     // Push the `Branch_i` instruction onto the instruction stack
            //     instr_stack.push(AgendaInstrs::Instructions(Instructions::Branch_i(br)));
            // },
            Stmt::WhileLoopStmt { pred, body, position } => {
                println!("in while loop");
                // Create new Loop_i instruction with cloned body expressions
                let mut lp = Loop_i {
                    body: body.clone(),
                };
                // Push Loop_i instruction onto instruction stack
                instr_stack.push(AgendaInstrs::Instructions(Instructions::Loop_i(lp.clone())));
                // Push predicate expression onto instruction stack
                instr_stack.push(AgendaInstrs::Expr(pred.clone()));
                // Push the predicate value onto the stash
                // stash.push(Literal::BoolLiteral(true));
            }
            _ => {
                println!("Not a function");
            }
        }
    }
}

impl Evaluate for Block {
    fn evaluate(&self, instr_stack: &mut Vec<AgendaInstrs>, stash: &mut Vec<Literal>, env: &mut Box<Environment>) {
        let outer = env.clone();
        let old_env = env.clone();
        if instr_stack.len() != 0 {
            instr_stack.push(AgendaInstrs::Environment(*old_env)); // Put environment on agenda
        }

        instr_stack.push(AgendaInstrs::Instructions(Instructions::Mark)); // Drop mark on agenda

        let mut locals = scan_out_block_declarations(self);
        let mut new_env = Environment::extend_environment(outer);

        new_env.insert_locals(locals);
        *env = Box::new(new_env);

        // TODO: Check for function declarations here and add to the environment
        let mut statements_clone = self.statements.clone();
        push_block_stmts_reverse(instr_stack, statements_clone);
    }
}

impl Evaluate for Expr {
    fn evaluate(&self, instr_stack: &mut Vec<AgendaInstrs>, stash: &mut Vec<Literal>, env: &mut Box<Environment>) {
        match self {
            Expr::IdentifierExpr(name, source_location) => {
                match env.get(name) { // Find identifier in the environment
                    Some(o) => {
                        let obj = o.clone();
                        let value = match obj { // Find identifier in pool
                            Object::Literal(lit) => lit.clone(),
                            _ => panic!("Identifier expr should point to literal only!")
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
                let arity = arguments.len();
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
                                let instr = App_i {
                                    arity,
                                    builtin: true,
                                    sym: String::from("println")
                                };
                                instr_stack.push(AgendaInstrs::Instructions(Instructions::App_i(instr)));
                            }
                        }
                    }
                } else {
                    let callee_copy = callee.clone();
                    let sym = match *callee_copy {
                        Expr::IdentifierExpr(i, ..) => i.clone(),
                        _ => panic!("Current implementation only supports identifiers as callees.")
                    };
                    let instr = App_i {
                        arity,
                        builtin: false,
                        sym
                    };
                    instr_stack.push(AgendaInstrs::Instructions(Instructions::App_i(instr)));
                }
                let mut arguments_clone = arguments.clone();
                let mut curr_expr: Option<Expr> = arguments_clone.pop();
                while curr_expr.is_some() { // Put arguments on agenda backwards
                    let curr: Expr = curr_expr.expect("No arguments");
                    instr_stack.push(AgendaInstrs::Expr(curr));
                    curr_expr = arguments_clone.pop();
                }
            }
            Expr::ReturnExpr(expression, source_location) => {}
        }
    }
}

impl Evaluate for PrimitiveOperation {
    fn evaluate(&self, instr_stack: &mut Vec<AgendaInstrs>, stash: &mut Vec<Literal>, env: &mut Box<Environment>) {
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
                //println!("???");
            },
        }
    }
}

impl Evaluate for Literal {
    fn evaluate(&self, instr_stack: &mut Vec<AgendaInstrs>, stash: &mut Vec<Literal>, env: &mut Box<Environment>) {
        match self {
            IntLiteral(n) => stash.push(IntLiteral(*n)), // Need to  extract the literal when using it
            BoolLiteral(b) => stash.push(BoolLiteral(*b)),
            StringLiteral(s) => {
                stash.push(StringLiteral(s.clone()));
            },
            UnitLiteral => { stash.push(UnitLiteral) },
            _ => panic!("This literal type is not supported!")
        }
    }
}