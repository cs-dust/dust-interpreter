use crate::parser::ast;
use crate::parser::ast::Literal::{BoolLiteral, IntLiteral, StringLiteral, UnitLiteral};
use std::cell::RefCell;
use std::collections::Hashmap;
use std::rc::Rc;

#[derive(PartialEq, Debug, Clone)]
pub struct Environment {
    store: HashMap<String, Literal>,
    parent: Option<Rc<RefCell<Environment>>>
}
