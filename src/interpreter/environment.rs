// Credits: Adapted from https://github.com/Rydgel/monkey-rust/blob/master/lib/evaluator/environment.rs
use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;
use crate::parser;
use crate::parser::ast::{DataType, Expr, PrimitiveOperation, SequenceStmt, Stmt, Block, Literal, UnaryOperator, BinaryOperator, VariadicOperator, PrimitiveOperator};
use crate::parser::ast::Literal::{BoolLiteral, IntLiteral, StringLiteral, UnitLiteral};

#[derive(Debug, Clone)]
pub struct TopLevelMap {
    pub map: HashMap<String, usize>,
    pub list: Vec<Stmt>,
    pub idx: usize
}

#[derive(Debug, Clone)]
pub struct Environment {
    pub store: HashMap<String, Object>,
    pub parent: Box<Link<Environment>>
}

// TODO: Replace to accommodate heap address instead
#[derive(Debug, Clone)]
pub enum Object {
    Literal(Literal),
    DeclStatement(Stmt)
}

impl Environment {
    pub fn new () -> Self {
        let mut hashmap = HashMap::new();
        Environment {
            store: hashmap,
            parent: None
        }
    }
    // Returns a new env which is a child of the previous env
    pub fn extend_environment(outer: Rc<RefCell<Environment>>) -> Self {
        let mut hashmap = HashMap::new();
        Environment {
            store: hashmap,
            parent: Some(outer)
        }
    }

    pub fn insert_top_level(&mut self, top_level_stmts: TopLevelMap) {
        let mut top_level_clone = top_level_stmts.clone();
        let mut curr_stmt: Option<Stmt> = top_level_clone.list.pop();
        while curr_stmt.is_some() {
            let curr: Stmt = curr_stmt.expect("Value");
            match curr.clone() {
                Stmt::StaticStmt { name, is_mutable, annotation, value, position } => {
                    let static_name: String = match name {
                        Expr::IdentifierExpr(name_string, sloc) => { name_string }
                        _ => panic!("No name?")
                    };
                    self.store.insert(static_name, Object::DeclStatement(curr));
                }
                Stmt::FuncDeclaration { name, lifetime_parameters, parameters, return_type, body, position } => {
                    let function_name: String = match name {
                        Expr::IdentifierExpr(name_string, sloc) => {
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


    pub fn set(&mut self, name: String, val: Object) {
        self.store.insert(name.to_string(), val);
    }

    pub fn get(&self, name: String) -> Option<Object> {
        match self.store.get(&*name.clone()) {
            Some(obj) => Some(obj.clone()),
            None => match self.parent {
                Some(ref parent_env) => {
                    let env = parent_env.borrow();
                    env.get(name)
                },
                None => None,
            },
        }
    }
}