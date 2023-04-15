// Credits: Adapted from https://github.com/Rydgel/monkey-rust/blob/master/lib/evaluator/environment.rs
use std::collections::HashMap;
use crate::interpreter::new_heap::Heap;
use crate::parser::ast::{Expr, Stmt, Literal};


#[derive(Debug, Clone)]
pub struct TopLevelMap {
    pub map: HashMap<String, usize>,
    pub list: Vec<Stmt>,
    pub idx: usize
}

#[derive(Debug, Clone)]
pub struct Environment {
    pub store: HashMap<String, Object>,
    parent: Option<Box<Environment>>,
}


// TODO: Replace to accommodate heap address instead
#[derive(Debug, Clone)]
pub enum Object {
    // Literal(Literal),
    DeclStatement(Stmt),
    PtrToLiteral(usize)
}

impl  Environment {
    pub fn new() -> Self {
        let hashmap = HashMap::new();
        Environment {
            store: hashmap,
            parent: None
        }
    }

    pub fn extend_environment(outer: Box<Environment>) -> Self {
        let hashmap = HashMap::new();
        Environment {
            store: hashmap,
            parent: Some(outer)
        }
    }

    pub fn set(&mut self, name: String, val: Object) {
        self.store.insert(name, val);
    }

    pub fn set_mut(&mut self, name: &str, val: Object) {
        println!("Searching and setting");
        match self.store.get(name) {
            Some(_obj) => {
                self.set(name.to_string(), val);
            },
            None => match self.parent {
                Some (ref mut parent_env) => {
                    parent_env.set_mut(name, val);
                },
                None => panic!("Value does not exist in environment!")
            }
        }
    }

    pub fn go_to_parent(env: Box<Environment>) -> Option<Box<Environment>> {
        env.parent
    }

    pub fn get(&mut self, name: &str) -> Option<Object> {
        match self.store.get(name) {
            Some (obj) => Some(obj.clone()),
            None => match self.parent {
                Some (ref mut parent_env) => {
                    parent_env.get(name)
                },
                None => None,
            },
        }
    }

    pub fn insert_top_level(&mut self, top_level_statements: TopLevelMap) {
        let mut top_level_clone = top_level_statements.clone();
        let mut curr_stmt: Option<Stmt> = top_level_clone.list.pop();
        while curr_stmt.is_some() {
            let curr: Stmt = curr_stmt.expect("Value");
            match curr.clone() {
                Stmt::StaticStmt { name, is_mutable: _, annotation: _, value: _, position: _ } => {
                    let static_name: String = match name {
                        Expr::IdentifierExpr(name_string, _sloc) => { name_string }
                        _ => panic!("No name?")
                    };
                    self.store.insert(static_name, Object::DeclStatement(curr));
                }
                Stmt::FuncDeclaration { name, lifetime_parameters: _, parameters: _, return_type: _, body: _, position: _ } => {
                    let function_name: String = match name {
                        Expr::IdentifierExpr(name_string, _sloc) => {
                            name_string
                        }
                        _ => panic!("No name??")
                    };
                    self.store.insert(function_name, Object::DeclStatement(curr));
                }
                _ => panic!("Only Function Declarations or Static Statements allowed in top level. ")
            };
            curr_stmt = top_level_clone.list.pop();
        }
    }

    pub fn insert_locals(&mut self, locals: Vec<String>, heap: &mut Heap) {
        let mut locals_clone = locals.clone();
        let mut curr_local: Option<String> = locals_clone.pop();

        while curr_local.is_some() { // Declare the locals
            let curr: String = curr_local.expect("No locals");
            let addr = heap.heap_push(Literal::UnitLiteral);
            self.set(curr, Object::PtrToLiteral(addr)); // TODO: INSERT HEAP Initialise to default Unit Literal Values done
            curr_local = locals_clone.pop();
        }
    }

    pub fn bind_parameters(&mut self, params: Vec<String>, args: Vec<Literal>, heap: &mut Heap) {
        let mut params_clone = params.clone();
        let mut curr_param_name: Option<String> = params_clone.pop();
        let mut i = 0;
        while curr_param_name.is_some() { // Bind passed values to the function parameters
            let curr: String = curr_param_name.expect("No params");
            let value_passed = args.get(i).expect("No value passed to a function that requires one?");
            match value_passed.clone() {
                Literal::StringRefLiteral(srf) => {
                    // equivalent to transfer_ownership in interpeter.rs
                    self.set(curr, Object::PtrToLiteral(srf.addr));
                    let mov_addr = heap.heap_push(Literal::MovedLiteral);
                    self.set_mut(srf.nam.as_str(), Object::PtrToLiteral(mov_addr));
                },
                _ => {
                    let addr = heap.heap_push(value_passed.clone());
                    self.set(curr, Object::PtrToLiteral(addr));
                }
            };
            i = i + 1;
            curr_param_name = params_clone.pop();
        }
    }
}