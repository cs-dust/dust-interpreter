mod environment;
mod new_heap;
use crate::interpreter::environment::Environment;
use crate::interpreter::new_heap::Heap;
use crate::parser;
use crate::parser::ast::Literal::{
    BoolLiteral, IntLiteral, MovedLiteral, StringLiteral, StringRefLiteral, UnitLiteral,
};
use crate::parser::ast::{
    BinaryOperator, Block, Expr, FuncParameter, Literal, PrimitiveOperation, PrimitiveOperator,
    SequenceStmt, Stmt, StringRef, UnaryOperator, VariadicOperator,
};
use environment::Object;
use environment::TopLevelMap;
use std::collections::HashMap;
use std::string::String;

#[derive(Debug, Clone)]
pub struct UnOp {
    sym: UnaryOperator,
}

#[derive(Debug, Clone)]
pub struct BinOp {
    sym: BinaryOperator,
}

#[derive(Debug, Clone)]
pub struct Assignment {
    sym: String,
    expr: Expr,
}

#[derive(Debug, Clone)]
pub struct AssignmentI {
    sym: String,
}

#[derive(Debug, Clone)]
pub struct AppI {
    arity: usize,
    builtin: bool,
    sym: String,
}

#[derive(Debug, Clone)]
pub struct BranchI {
    cons: Expr,
    alt: Option<Expr>,
}

#[derive(Debug, Clone)]
pub struct LoopI {
    body: Expr,
    pred: Expr,
}

#[derive(Debug, Clone)]
pub struct Overwrite {
    sym: String,
    expr: Expr,
}

#[derive(Debug, Clone)]
pub struct OverwriteI {
    sym: String,
}

#[derive(Debug, Clone)]
pub enum Instructions {
    Reset,
    Mark,
    Assignment(Assignment),
    UnOp(UnOp),
    BinOp(BinOp),
    Pop,
    AssignmentI(AssignmentI),
    AppI(AppI),
    BranchI(BranchI),
    LoopI(LoopI),
    Overwrite(Overwrite),
    OverwriteI(OverwriteI),
}

#[derive(Debug, Clone)]
enum AgendaInstrs {
    Instructions(Instructions),
    Stmt(Stmt),
    Block(Block),
    // SequenceStmt(SequenceStmt),
    Literal(Literal),
    Expr(Expr),
    PrimitiveOperation(PrimitiveOperation),
    Environment(Box<Environment>),
}

/***************************************************************************************************
* Run the code
***************************************************************************************************/
pub fn run(ast: &mut Vec<parser::ast::Stmt>, debug: bool) {
    // To be used later, commented to suppress warnings
    // The heap for the program being run by our interpreter
    // let heap = Heap::new();

    // A list of functions in the interpreted program
    // The vector stores all the functions as Stmt values
    // The hashmap allows us to find a function by a string label TODO: remove map, not needed anymore
    let mut functions = TopLevelMap {
        map: HashMap::new(),
        list: Vec::new(),
        idx: 0,
    };

    // Similar to above, but for statics
    let mut statics = TopLevelMap {
        map: HashMap::new(),
        list: Vec::new(),
        idx: 0,
    };

    // Pop a statement from our ast
    let mut curr_stmt_option: Option<Stmt> = ast.pop();

    // While we have a statement to evaluate
    while curr_stmt_option.is_some() {
        let curr: Stmt = curr_stmt_option.expect("How is this even possible bruh");
        // add a static or a function to the above lists as necessary.
        // any other type of statement is not allowed in the top level.
        match curr.clone() {
            Stmt::StaticStmt {
                name,
                is_mutable: _,
                annotation: _,
                value: _,
                position: _,
            } => {
                statics.list.push(curr);
                // check if there is a name
                let static_name: String = match name {
                    // there is a name
                    Expr::IdentifierExpr(name_string, _sloc) => {
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
            Stmt::FuncDeclaration {
                name,
                lifetime_parameters: _,
                parameters: _,
                return_type: _,
                body: _,
                position: _,
            } => {
                functions.list.push(curr);
                // check if there is a name
                let function_name: String = match name {
                    // there is a name
                    Expr::IdentifierExpr(name_string, _sloc) => {
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

    if debug {
        println!("{} function(s) have been declared", functions.idx);
        println!("main function index: {}", main_idx);
    }

    let mut agenda: Vec<AgendaInstrs> = Vec::new(); // A is our stack of instructions as in the Source EC evaluator
    let mut stash: Vec<Literal> = Vec::new(); // S is the stash of evaluated values
    let mut global_env = Environment::new(debug);
    global_env.insert_top_level(functions.clone()); // Insert top level declarations into environment
    global_env.insert_top_level(statics.clone());
    let mut environment = Box::new(global_env);
    let mut heap = Heap::new(debug);
    heap.clear_heap();
    agenda.push(AgendaInstrs::Stmt(functions.list[main_idx].clone())); // We start with main
                                                                       // TODO: Check if we should have a step limit
    while !agenda.is_empty() {
        let curr: AgendaInstrs = agenda.pop().expect("Literally impossible but ok");
        curr.evaluate(&mut agenda, &mut stash, &mut environment, &mut heap, debug);
        // Borrowing w/ Mutable Reference
        //println!("{:#?}", curr.clone());
    }
    if stash.is_empty() || stash.len() > 1 {
        println!("Stash is empty!");
    } else {
        println!("{:#?}", stash[0]);
    }
}

fn print_statement(args: Vec<Literal>, heap: &Heap) {
    for arg in args.iter() {
        match arg {
            StringLiteral(s) => {
                print!("{}", s);
            }
            IntLiteral(i) => {
                print!("{}", i);
            }
            BoolLiteral(b) => {
                print!("{}", b);
            }
            UnitLiteral => {
                print!("UnitLiteral")
            }
            MovedLiteral => {
                print!("\t!!!This value was moved!!!\t")
            }
            StringRefLiteral(sr) => match heap.heap_get(sr.addr) {
                StringLiteral(s) => {
                    print!("{}", s)
                }
                _ => {
                    panic!()
                }
            },
        }
    }
    println!("");
}

/***************************************************************************************************
* Operators
***************************************************************************************************/

pub fn binop_microcode_num(x: i64, y: i64, sym: BinaryOperator) -> Literal {
    let output = match sym {
        BinaryOperator::Plus => x + y,
        BinaryOperator::Minus => x - y,
        BinaryOperator::Times => x * y,
        BinaryOperator::Divide => x / y,
        _ => panic!("fn. binop_microcode_num operation unsupported!"),
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
        _ => panic!("fn. binop_microcode_num_bool operation unsupported!"),
    };
    return BoolLiteral(output);
}

pub fn binop_microcode_bool(x: bool, y: bool, sym: BinaryOperator) -> Literal {
    let output = match sym {
        BinaryOperator::And => x && y,
        BinaryOperator::Or => x || y,
        _ => panic!("fn. binop_microcode_bool operation unsupported!"),
    };
    return BoolLiteral(output);
}

pub fn apply_binop(
    x: Option<Literal>,
    y: Option<Literal>,
    sym: BinaryOperator,
    heap: &mut Heap,
    _env: &mut Box<Environment>,
) -> Literal {
    //print!("{:#?}", x);
    let output = match x {
        // Check type of x
        Some(Literal::IntLiteral(x_num)) => match y {
            Some(Literal::IntLiteral(y_num)) => match sym {
                // Ensure x & y are both integer types
                BinaryOperator::Plus
                | BinaryOperator::Minus
                | BinaryOperator::Divide
                | BinaryOperator::Times => binop_microcode_num(x_num, y_num, sym),
                BinaryOperator::Equal
                | BinaryOperator::NotEqual
                | BinaryOperator::Greater
                | BinaryOperator::GreaterOrEqual
                | BinaryOperator::Less
                | BinaryOperator::LessOrEqual => binop_microcode_num_bool(x_num, y_num, sym),
                _ => panic!("fn. apply_binop operator unsupported for types x(int) and y(int)!"),
            },
            _ => panic!("fn. apply_binop x(int) and y have different types!"),
        },
        Some(Literal::BoolLiteral(x_bool)) => match y {
            Some(Literal::BoolLiteral(y_bool)) => match sym {
                // Ensure x any y are both bool types
                BinaryOperator::And | BinaryOperator::Or => {
                    binop_microcode_bool(x_bool, y_bool, sym)
                }
                _ => panic!("fn. apply_binop operator unsupported for types x(bool) and y(bool)!"),
            },
            _ => panic!("fn. apply_binop x(bool) and y have different types!"),
        },
        Some(Literal::StringRefLiteral(srfx)) => match y {
            Some(Literal::StringRefLiteral(srfy)) => match sym {
                BinaryOperator::Plus => {
                    let concat_str_addr = heap.heap_string_concat(srfx.addr, srfy.addr);
                    let concat_str = match heap.heap_get(concat_str_addr) {
                        Literal::StringLiteral(s) => s,
                        _ => panic!("How did a string concat to this?"),
                    };
                    let new_str = StringRef {
                        value: concat_str,
                        addr: concat_str_addr,
                        nam: srfx.nam, // x moves
                    };
                    Literal::StringRefLiteral(new_str)
                }
                _ => panic!("fn apply_binop does not support given operator for strings"),
            },
            Some(Literal::StringLiteral(y_str)) => match sym {
                BinaryOperator::Plus => {
                    let y_str_addr = heap.heap_push(Literal::StringLiteral(y_str));
                    let concat_str_addr = heap.heap_string_concat(srfx.addr, y_str_addr);
                    let concat_str = match heap.heap_get(concat_str_addr) {
                        Literal::StringLiteral(s) => s,
                        _ => panic!("How did a string concat to this?"),
                    };
                    heap.free_space(y_str_addr); // Clear y, not used again TODO: Fix
                    let new_str = StringRef {
                        value: concat_str,
                        addr: concat_str_addr,
                        nam: srfx.nam, // x moves
                    };
                    Literal::StringRefLiteral(new_str)
                }
                _ => panic!("fn. apply_binop does not support given operator for strings"),
            },
            _ => panic!("fn. apply_binop does not support given operator for the given types"),
        },
        _ => panic!("fn. apply_binop primitive operations unsupported for this type!"),
    };
    return output;
}

pub fn unop_microcode_bool(x: bool, sym: UnaryOperator) -> Literal {
    let output = match sym {
        UnaryOperator::Not => !x,
        _ => panic!("fn. unop_microcode_bool unsupported operator!"),
    };
    return BoolLiteral(output);
}

pub fn unop_microcode_num(x: i64, sym: UnaryOperator) -> Literal {
    let output = match sym {
        UnaryOperator::UnaryMinus => -x,
        _ => panic!("fn. unop_microcode_num unsupported operator!"),
    };
    return IntLiteral(output);
}

pub fn apply_unop(x: Option<Literal>, sym: UnaryOperator) -> Literal {
    let output = match x {
        Some(Literal::IntLiteral(value)) => unop_microcode_num(value, sym),
        Some(Literal::BoolLiteral(value)) => unop_microcode_bool(value, sym),
        _ => panic!("fn. apply_unop unsupported type for x!"),
    };
    return output;
}

/***************************************************************************************************
* Pushing statements onto agenda in reverse
***************************************************************************************************/
fn push_block_stmts_reverse(
    instr_stack: &mut Vec<AgendaInstrs>,
    statements: Vec<SequenceStmt>,
    env: &mut Box<Environment>,
) {
    let mut statements_clone = statements.clone();
    let mut curr_stmt = statements_clone.pop();
    while curr_stmt.is_some() {
        let curr: SequenceStmt = curr_stmt.expect("No block statements?"); // current statement
        match curr.clone() {
            SequenceStmt::Stmt(s) => match s.clone() {
                Stmt::FuncDeclaration {
                    name,
                    lifetime_parameters: _,
                    parameters: _,
                    return_type: _,
                    body: _,
                    position: _,
                } => {
                    let nam = get_name(&name);
                    env.set(nam, Object::DeclStatement(s));
                }
                _ => {
                    instr_stack.push(AgendaInstrs::Stmt(s));
                    instr_stack.push(AgendaInstrs::Instructions(Instructions::Pop));
                }
            },
            SequenceStmt::Block(b) => instr_stack.push(AgendaInstrs::Block(b)),
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
        let (expr, _datatype) = param;
        let name = get_name(expr);
        vec![name]
    };

    params
        .iter()
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
        _ => vec![],
    };

    stmts
        .iter()
        .map(scanned_stmt)
        .fold(vec![], |accumulator, element| {
            // fold is used to convert a collection of items into a single set
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
    let block_stmts: Vec<Stmt> =
        block
            .statements
            .iter()
            .fold(vec![], |mut accumulate_stmts, seq_stmt| match seq_stmt {
                SequenceStmt::Stmt(stmt) => {
                    accumulate_stmts.push(stmt.clone());
                    accumulate_stmts
                }
                _ => accumulate_stmts,
            });
    scan_out_declarations(&block_stmts)
}

/***************************************************************************************************
* Get the identifier name
***************************************************************************************************/
fn get_name(expr: &Expr) -> String {
    match expr {
        Expr::IdentifierExpr(name, _sourcelocation) => name.clone(),
        _ => panic!("fn. get_name could not find identifier to get name from"),
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
* Ownership
***************************************************************************************************/
pub fn transfer_ownership(
    from: String,
    to: String,
    from_addr: usize,
    set_mut: bool,
    env: &mut Box<Environment>,
    heap: &mut Heap,
) {
    // Set the 'to' value to the address of the 'from' binding
    if set_mut {
        env.set_mut(to.as_str(), Object::PtrToLiteral(from_addr));
    } else {
        env.set(to, Object::PtrToLiteral(from_addr));
    }
    let mov_addr = heap.heap_push(Literal::MovedLiteral);
    // Set the 'from' variable to moved (To show proof of concept)
    env.set_mut(from.as_str(), Object::PtrToLiteral(mov_addr));
}

/***************************************************************************************************
* Evaluation
***************************************************************************************************/
trait Evaluate {
    fn evaluate(
        &self,
        instr_stack: &mut Vec<AgendaInstrs>,
        stash: &mut Vec<Literal>,
        env: &mut Box<Environment>,
        heap: &mut Heap,
        debug: bool,
    );
}

impl Evaluate for AgendaInstrs {
    fn evaluate(
        &self,
        instr_stack: &mut Vec<AgendaInstrs>,
        stash: &mut Vec<Literal>,
        env: &mut Box<Environment>,
        heap: &mut Heap,
        debug: bool,
    ) {
        // let instr_ptr = instr_stack.len();
        match self {
            AgendaInstrs::Stmt(stmt) => stmt.evaluate(instr_stack, stash, env, heap, debug),
            // AgendaInstrs::SequenceStmt(stmts) => stmts.evaluate(instr_stack, stash),
            AgendaInstrs::PrimitiveOperation(prim_op) => {
                prim_op.evaluate(instr_stack, stash, env, heap, debug)
            }
            AgendaInstrs::Block(blk) => blk.evaluate(instr_stack, stash, env, heap, debug),
            AgendaInstrs::Literal(lit) => lit.evaluate(instr_stack, stash, env, heap, debug),
            AgendaInstrs::Expr(expr) => expr.evaluate(instr_stack, stash, env, heap, debug),
            AgendaInstrs::Environment(ref _e) => {
                // Restore environment
                if debug {
                    println!("Restoring");
                }
                let with_outer = env.clone();
                for (_, value) in with_outer.store.iter() {
                    match value {
                        Object::PtrToLiteral(addr) => {
                            if debug {
                                println!("---heap before---");
                                heap.print_stats();
                            }
                            heap.free_space(*addr);
                            if debug {
                                println!("---heap after---");
                                heap.print_stats();
                                println!("----------------");
                            }
                        }
                        _ => {}
                    };
                }
                let inner = Environment::go_to_parent(with_outer);
                match inner.clone() {
                    Some(p) => {
                        *env = p;
                    }
                    None => println!("Environment was not restored. Using old env."),
                }
                //println!("{:#?}", env.clone());
            }
            AgendaInstrs::Instructions(instr) => {
                match instr {
                    Instructions::Reset => {
                        match instr_stack.pop() {
                            Some(AgendaInstrs::Instructions(Instructions::Mark)) => {
                                // Mark found, now environment restoration occurs
                            }
                            _ => instr_stack.push(AgendaInstrs::Instructions(Instructions::Reset)), // Continue loop by pushing reset back onto agenda
                        }
                    }
                    Instructions::Assignment(assn) => {
                        let a = AssignmentI {
                            sym: assn.clone().sym,
                        };
                        instr_stack.push(AgendaInstrs::Instructions(Instructions::AssignmentI(a)));
                        instr_stack.push(AgendaInstrs::Expr(assn.clone().expr));
                    }
                    Instructions::Overwrite(ovr) => {
                        let o = OverwriteI {
                            sym: ovr.clone().sym,
                        };
                        instr_stack.push(AgendaInstrs::Instructions(Instructions::OverwriteI(o)));
                        instr_stack.push(AgendaInstrs::Expr(ovr.clone().expr));
                    }
                    Instructions::UnOp(unop) => {
                        let operand = stash.pop();
                        let operator = unop.sym;
                        let value = apply_unop(operand, operator);
                        stash.push(value);
                    }
                    Instructions::BinOp(binop) => {
                        let rhs_operand = stash.pop();
                        let lhs_operand = stash.pop();
                        let operator = binop.sym;
                        let value = apply_binop(lhs_operand, rhs_operand, operator, heap, env);
                        stash.push(value);
                    }
                    Instructions::Pop => {
                        stash.pop();
                    }
                    Instructions::Mark => {
                        // Nothing
                    }
                    Instructions::AssignmentI(assn) => {
                        let nam = assn.clone().sym;
                        let v = match stash.peek() {
                            Some(value) => value.clone(),
                            None => panic!("Why is nothing in the stash??"),
                        };
                        // Check if the rhs is a string ref, which means that string ownership is going to move
                        match v.clone() {
                            Literal::StringRefLiteral(srf) => {
                                transfer_ownership(srf.nam, nam, srf.addr, false, env, heap);
                            }
                            _ => {
                                let addr = heap.heap_push(v);
                                env.set(nam, Object::PtrToLiteral(addr)); // Store the address of the value in the heap
                            }
                        };
                    }
                    Instructions::OverwriteI(ovr) => {
                        // Overwrite name in the environment
                        let nam = ovr.clone().sym;
                        // Get the address of where it is currently stored
                        let old_addr = match env.get(nam.clone().as_str()) {
                            Some(Object::PtrToLiteral(ptr)) => ptr,
                            _ => {
                                panic!()
                            }
                        };
                        heap.free_space(old_addr);

                        let v = match stash.peek() {
                            Some(value) => value.clone(),
                            None => panic!("Why is nothing in the stash??"),
                        };
                        match v.clone() {
                            Literal::StringRefLiteral(srf) => {
                                transfer_ownership(srf.nam, nam, srf.addr, true, env, heap);
                            }
                            _ => {
                                let addr = heap.heap_push(v);
                                env.set_mut(nam.as_str(), Object::PtrToLiteral(addr));
                            }
                        };
                    }
                    Instructions::AppI(app) => {
                        let arity = app.arity;
                        let args = {
                            if arity == 0 {
                                vec![]
                            } else {
                                let mut arguments = vec![Literal::UnitLiteral; arity];
                                for i in (0..=(arity - 1)).rev() {
                                    arguments[i] =
                                        stash.pop().expect("Value should be here if arity is > 0");
                                }
                                arguments
                            }
                        };
                        if app.builtin {
                            match app.sym.as_str() {
                                "println" => print_statement(args, heap),
                                _ => panic!("Builtin {} not supported!", app.sym.as_str()),
                            }
                        } else {
                            let store_env = match instr_stack.peek() {
                                Some(AgendaInstrs::Environment(_e)) => true,
                                _ => false,
                            };
                            let tail_call = match instr_stack.peek() {
                                Some(AgendaInstrs::Instructions(Instructions::Reset)) => true,
                                _ => false,
                            };
                            if instr_stack.len() == 0 || store_env {
                                // Env is not needed, just push mark
                                instr_stack.push(AgendaInstrs::Instructions(Instructions::Mark));
                            } else if tail_call {
                                // Tail call, callee's return will push another reset
                                instr_stack.pop();
                            } else {
                                let old_env = env.clone();
                                instr_stack.push(AgendaInstrs::Environment(old_env)); // Put environment on agenda EDITED
                                instr_stack.push(AgendaInstrs::Instructions(Instructions::Mark));
                            }
                            let func_stmt = match env.get(&*app.sym.clone()) {
                                Some(Object::DeclStatement(s)) => s.clone(),
                                _ => panic!("Function not declared in scope!"),
                            };
                            let params;
                            let fun_body;
                            match func_stmt.clone() {
                                Stmt::FuncDeclaration {
                                    name: _,
                                    lifetime_parameters: _,
                                    parameters,
                                    return_type: _,
                                    body,
                                    position: _,
                                } => {
                                    params = parameters;
                                    fun_body = body;
                                }
                                _ => {
                                    panic!("Why does the function not have a function declaration?")
                                }
                            };
                            // Push the body of the function onto the agenda backwards
                            // Extend the environment with the new vars
                            let param_names = scan_out_param_names(&params);
                            let locals = scan_out_block_declarations(&fun_body);

                            let outer = env.clone();
                            let mut new_env = Environment::extend_environment(outer);

                            new_env.bind_parameters(param_names, args, heap);
                            new_env.insert_locals(locals, heap);
                            *env = Box::new(new_env); // Change the current env

                            // TODO: Check for function declarations here and add to the environment
                            let statements = fun_body.statements.clone();
                            push_block_stmts_reverse(instr_stack, statements, env);
                        }
                    }
                    Instructions::BranchI(br) => {
                        let cons = br.cons.clone();
                        let alt = br.alt.clone();

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
                                        Some(alt_expr) => {
                                            instr_stack.push(AgendaInstrs::Expr(alt_expr))
                                        }
                                        // Do nothing if no alt
                                        None => {}
                                    }
                                }
                            }
                            _ => panic!("Predicate type not supported"),
                        }
                    }
                    Instructions::LoopI(lp) => {
                        let body = lp.body.clone();
                        let pred_val = stash.pop().expect("Expected predicate value on stash");
                        match pred_val {
                            Literal::BoolLiteral(b) => {
                                if b {
                                    // Push Loop_i tag back onto agenda
                                    instr_stack.push(AgendaInstrs::Instructions(
                                        Instructions::LoopI(lp.clone()),
                                    ));

                                    // Push pred expr onto the instruction stack
                                    instr_stack.push(AgendaInstrs::Expr(lp.pred.clone()));

                                    // Push POP tag onto the agenda
                                    instr_stack.push(AgendaInstrs::Instructions(Instructions::Pop));

                                    // Push body of loop onto the instruction stack
                                    instr_stack.push(AgendaInstrs::Expr(body));
                                }
                            }
                            _ => {
                                panic!("Unexpected result");
                            }
                        }
                    }
                }
            } // _ => println!("Agenda is empty"),
        }
    }
}

impl Evaluate for Stmt {
    fn evaluate(
        &self,
        instr_stack: &mut Vec<AgendaInstrs>,
        _stash: &mut Vec<Literal>,
        _env: &mut Box<Environment>,
        _heap: &mut Heap,
        debug: bool,
    ) {
        match self {
            Stmt::LetStmt {
                name,
                is_mutable: _,
                annotation: _,
                value,
                position: _,
            } => match value {
                Some(expr) => {
                    let name = get_name(name);
                    // TODO: Put name in the environment (?)
                    instr_stack.push(AgendaInstrs::Literal(Literal::UnitLiteral));
                    instr_stack.push(AgendaInstrs::Instructions(Instructions::Pop));
                    let a = Assignment {
                        sym: name.clone(),
                        expr: expr.clone(),
                    };
                    instr_stack.push(AgendaInstrs::Instructions(Instructions::Assignment(a)));
                }
                None => panic!("Unbounded declaration is currently unsupported!"),
            },
            Stmt::FuncDeclaration {
                name: _,
                lifetime_parameters: _,
                parameters: _,
                return_type: _,
                body,
                position: _,
            } => {
                instr_stack.push(AgendaInstrs::Block(body.clone()));
            }
            Stmt::ExprStmt(expr) => match expr {
                Expr::ReturnExpr(expr, _loc) => {
                    instr_stack.push(AgendaInstrs::Instructions(Instructions::Reset));
                    let expr_clone = expr.clone();
                    instr_stack.push(AgendaInstrs::Expr(*expr_clone));
                }
                _ => {
                    instr_stack.push(AgendaInstrs::Literal(Literal::UnitLiteral));
                    instr_stack.push(AgendaInstrs::Instructions(Instructions::Pop));
                    instr_stack.push(AgendaInstrs::Expr(expr.clone()));
                }
            },
            Stmt::IfElseStmt {
                pred,
                cons,
                alt,
                position: _,
            } => {
                // Create new Branch_i instruction with cloned cons and alt expressions
                let br = BranchI {
                    cons: cons.clone(),
                    alt: alt.clone(),
                };
                // Push Branch_i instruction onto instruction stack
                instr_stack.push(AgendaInstrs::Instructions(Instructions::BranchI(br)));
                // // Push predicate expression onto instruction stack
                instr_stack.push(AgendaInstrs::Expr(pred.clone()));
            }
            Stmt::WhileLoopStmt {
                pred,
                body,
                position: _,
            } => {
                if debug {
                    println!("in while loop");
                }
                // Create new Loop_i instruction with cloned body expressions
                let lp = LoopI {
                    pred: pred.clone(),
                    body: body.clone(),
                };

                // Push Loop_i instruction onto instruction stack
                instr_stack.push(AgendaInstrs::Instructions(Instructions::LoopI(lp.clone())));

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
    fn evaluate(
        &self,
        instr_stack: &mut Vec<AgendaInstrs>,
        _stash: &mut Vec<Literal>,
        env: &mut Box<Environment>,
        heap: &mut Heap,
        _debug: bool,
    ) {
        let outer = env.clone();
        let old_env = env.clone();
        if instr_stack.len() != 0 {
            instr_stack.push(AgendaInstrs::Environment(old_env)); // Put environment on agenda EDITED
        }

        instr_stack.push(AgendaInstrs::Instructions(Instructions::Mark)); // Drop mark on agenda

        let locals = scan_out_block_declarations(self);
        let mut new_env = Environment::extend_environment(outer);

        new_env.insert_locals(locals, heap);
        *env = Box::new(new_env);

        // TODO: Check for function declarations here and add to the environment
        let statements_clone = self.statements.clone();
        push_block_stmts_reverse(instr_stack, statements_clone, env);
    }
}

impl Evaluate for Expr {
    fn evaluate(
        &self,
        instr_stack: &mut Vec<AgendaInstrs>,
        stash: &mut Vec<Literal>,
        env: &mut Box<Environment>,
        heap: &mut Heap,
        debug: bool,
    ) {
        match self {
            Expr::IdentifierExpr(name, _source_location) => {
                match env.get(name) {
                    // Find identifier in the environment
                    Some(o) => {
                        let obj = o.clone();
                        let address;
                        let mut value = match obj {
                            // Find identifier in pool
                            // Object::Literal(lit) => lit.clone(),
                            Object::PtrToLiteral(addr) => {
                                address = addr;
                                heap.heap_get(addr)
                            }
                            _ => panic!("Identifier expr should point to literal only!"),
                        };
                        // Check if this references a String, obtain address
                        // If it is a string, push the reference to it onto the stash as well
                        value = match value.clone() {
                            Literal::StringLiteral(s) => {
                                let str = StringRef {
                                    value: s,
                                    addr: address,
                                    nam: name.clone(),
                                };
                                Literal::StringRefLiteral(str)
                            }
                            _ => value,
                        };
                        stash.push(value);
                    }
                    None => {
                        panic!("evaluate identifier expr: Identifier not found in environment!");
                    }
                }
            }
            Expr::LiteralExpr(literal_value, _source_location) => {
                literal_value.evaluate(instr_stack, stash, env, heap, debug);
            }
            Expr::BlockExpr(block, _source_location) => {
                // Block expr need to have a return stmt
                block.evaluate(instr_stack, stash, env, heap, debug);
            }
            Expr::PrimitiveOperationExpr(primitive_op, _source_location) => {
                let prim_op = *primitive_op.clone();
                instr_stack.push(AgendaInstrs::PrimitiveOperation(prim_op));
            }
            Expr::AssignmentExpr {
                assignee,
                value,
                position: _,
            } => {
                let name = get_name(assignee);
                let expr = *value.clone();
                instr_stack.push(AgendaInstrs::Literal(Literal::UnitLiteral));
                instr_stack.push(AgendaInstrs::Instructions(Instructions::Pop));
                let o = Overwrite {
                    sym: name,
                    expr: expr,
                };
                instr_stack.push(AgendaInstrs::Instructions(Instructions::Overwrite(o)));
            }
            Expr::ApplicationExpr {
                is_primitive,
                callee,
                arguments,
                position: _,
            } => {
                let arity = arguments.len();
                if is_primitive.is_some() {
                    match is_primitive.unwrap() {
                        PrimitiveOperator::Unary(op) => match op {
                            UnaryOperator::ImmutableBorrow => {}
                            UnaryOperator::MutableBorrow => {}
                            UnaryOperator::Dereference => {}
                            UnaryOperator::StringFrom => {}
                            UnaryOperator::Drop => {}
                            UnaryOperator::Len => {}
                            UnaryOperator::AsStr => {}
                            UnaryOperator::PushStr => {}
                            _ => panic!("Evaluate application: Unknown primitive function!"),
                        },
                        PrimitiveOperator::Binary(_op) => {
                            panic!("Evaluate application: Unknown primitive function!")
                        }
                        PrimitiveOperator::VariadicOperator(vo) => match vo {
                            VariadicOperator::Println => {
                                let instr = AppI {
                                    arity,
                                    builtin: true,
                                    sym: String::from("println"),
                                };
                                instr_stack
                                    .push(AgendaInstrs::Instructions(Instructions::AppI(instr)));
                            }
                        },
                    }
                } else {
                    let callee_copy = callee.clone();
                    let sym = match *callee_copy {
                        Expr::IdentifierExpr(i, ..) => i.clone(),
                        _ => panic!("Current implementation only supports identifiers as callees."),
                    };
                    let instr = AppI {
                        arity,
                        builtin: false,
                        sym,
                    };
                    instr_stack.push(AgendaInstrs::Instructions(Instructions::AppI(instr)));
                }
                let mut arguments_clone = arguments.clone();
                let mut curr_expr: Option<Expr> = arguments_clone.pop();
                while curr_expr.is_some() {
                    // Put arguments on agenda backwards
                    let curr: Expr = curr_expr.expect("No arguments");
                    instr_stack.push(AgendaInstrs::Expr(curr));
                    curr_expr = arguments_clone.pop();
                }
            }
            Expr::ReturnExpr(_expression, _source_location) => {}
        }
    }
}

impl Evaluate for PrimitiveOperation {
    fn evaluate(
        &self,
        instr_stack: &mut Vec<AgendaInstrs>,
        _stash: &mut Vec<Literal>,
        _env: &mut Box<Environment>,
        _heap: &mut Heap,
        _debug: bool,
    ) {
        match self {
            PrimitiveOperation::UnaryOperation { operator, operand } => {
                let un_operator = UnOp {
                    sym: operator.clone(),
                };
                let un_operand = operand.clone();
                instr_stack.push(AgendaInstrs::Instructions(Instructions::UnOp(un_operator)));
                instr_stack.push(AgendaInstrs::Expr(un_operand));
            }
            PrimitiveOperation::BinaryOperation {
                operator,
                first_operand,
                second_operand,
            } => {
                let bin_operator = BinOp {
                    sym: operator.clone(),
                };
                let lhs_operand = first_operand.clone();
                let rhs_operand = second_operand.clone();
                instr_stack.push(AgendaInstrs::Instructions(Instructions::BinOp(
                    bin_operator,
                )));
                instr_stack.push(AgendaInstrs::Expr(rhs_operand));
                instr_stack.push(AgendaInstrs::Expr(lhs_operand));
            }
            PrimitiveOperation::VariadicOperation {
                operator: _,
                operands: _,
            } => {
                // TODO
                //println!("???");
            }
        }
    }
}

impl Evaluate for Literal {
    fn evaluate(
        &self,
        _instr_stack: &mut Vec<AgendaInstrs>,
        stash: &mut Vec<Literal>,
        _env: &mut Box<Environment>,
        _heap: &mut Heap,
        _debug: bool,
    ) {
        match self {
            IntLiteral(n) => stash.push(IntLiteral(*n)), // Need to  extract the literal when using it
            BoolLiteral(b) => stash.push(BoolLiteral(*b)),
            StringLiteral(s) => {
                stash.push(StringLiteral(s.clone()));
            }
            UnitLiteral => stash.push(UnitLiteral),
            StringRefLiteral(srf) => {
                stash.push(StringRefLiteral(srf.clone()));
            }
            MovedLiteral => stash.push(MovedLiteral),
        }
    }
}
