// Initial: For testing, will change acc to our requirements
// oxido-ast
// Author: @chuabingquan for https://github.com/cs4215-seville/oxido-lang
//
#[allow(dead_code)]
use std::fmt::Debug;

pub trait AST {
    fn get_source_location(&self) -> SourceLocation;
}

#[derive(Debug, Copy, Clone)]
pub struct SourceLocation {
    pub line: usize,
    pub col: usize,
}

pub type LifetimeParameter = String;

#[derive(Debug, Clone)]
pub enum DataType {
    Int64,
    Bool,
    Str,
    String,
    Unit,
    Ref(Option<LifetimeParameter>, Box<DataType>),
    MutRef(Option<LifetimeParameter>, Box<DataType>),
    Func(Vec<LifetimeParameter>, Vec<DataType>, Box<DataType>)
}


#[derive(Debug, Clone)]
pub enum Literal {
    IntLiteral(i64),
    BoolLiteral(bool),
    StringLiteral(String),
    UnitLiteral
}

#[derive(Debug, Clone)]
pub enum SequenceStmt {
    Stmt(Stmt),
    Block(Block),
}

pub type Sequence = Vec<SequenceStmt>;

#[derive(Debug, Clone)]
pub struct Block {
    pub statements: Sequence,
}

#[derive(Debug, Clone)]
pub enum Expr {
    IdentifierExpr(String, SourceLocation),
    LiteralExpr(Literal, SourceLocation),
    BlockExpr(Box<Block>, SourceLocation),
    PrimitiveOperationExpr(Box<PrimitiveOperation>, SourceLocation),
    AssignmentExpr {
        assignee: Box<Expr>,
        value: Box<Expr>,
        position: SourceLocation,
    },
    ApplicationExpr {
        is_primitive: Option<PrimitiveOperator>,
        callee: Box<Expr>,
        arguments: Vec<Expr>,
        position: SourceLocation,
    },
    ReturnExpr(Box<Expr>, SourceLocation),
}

impl AST for Expr {
    fn get_source_location(&self) -> SourceLocation {
        match self {
            Expr::IdentifierExpr(_, position) => position.clone(),
            Expr::LiteralExpr(_, position) => position.clone(),
            Expr::BlockExpr(_, position) => position.clone(),
            Expr::PrimitiveOperationExpr(_, position) => position.clone(),
            Expr::AssignmentExpr { position, .. } => position.clone(),
            Expr::ApplicationExpr { position, .. } => position.clone(),
            Expr::ReturnExpr(_, position) => position.clone(),
        }
    }
}

#[derive(Debug, Clone)]
pub enum PrimitiveOperation {
    UnaryOperation {
        operator: UnaryOperator,
        operand: Expr,
    },
    BinaryOperation {
        operator: BinaryOperator,
        first_operand: Expr,
        second_operand: Expr,
    },
    VariadicOperation {
        operator: VariadicOperator,
        operands: Vec<Expr>,
    }
}

#[derive(Debug, Copy, Clone)]
pub enum PrimitiveOperator {
    // Nullary(NullaryOperator),
    Unary(UnaryOperator),
    Binary(BinaryOperator),
    VariadicOperator(VariadicOperator),
}

// #[derive(Debug, Copy, Clone)]
// pub enum NullaryOperator {
//     Main,
// }

#[derive(Debug, Copy, Clone)]
pub enum UnaryOperator {
    Not,
    UnaryMinus,
    ImmutableBorrow,
    MutableBorrow,
    Dereference,
    StringFrom,
    Drop,
    Len,
    AsStr,
    PushStr,
}

#[derive(Debug, Copy, Clone)]
pub enum BinaryOperator {
    Plus,
    Minus,
    Times,
    Divide,
    Equal,
    NotEqual,
    Greater,
    GreaterOrEqual,
    Less,
    LessOrEqual,
    And,
    Or,
}

#[derive(Debug, Copy, Clone)]
pub enum VariadicOperator {
    Println,
}

pub type FuncParameter = (Expr, DataType);

#[derive(Debug, Clone)]
pub enum Stmt {
    LetStmt {
        name: Expr,
        is_mutable: bool,
        annotation: Option<DataType>,
        value: Option<Expr>,
        position: SourceLocation,
    },
    StaticStmt {
        name: Expr,
        is_mutable: bool,
        annotation: DataType,
        value: Expr,
        position: SourceLocation,
    },
    FuncDeclaration {
        name: Expr,
        lifetime_parameters: Vec<LifetimeParameter>,
        parameters: Vec<FuncParameter>,
        return_type: DataType,
        body: Block,
        position: SourceLocation,
    },
    ExprStmt(Expr),
    IfElseStmt {
        pred: Expr,
        cons: Expr,
        alt: Option<Expr>,     // add optional block for alt
        position: SourceLocation,
    },
    ForLoopStmt {
        init: Expr,
        pred: Expr,
        update: Expr,
        body: Expr,
        position: SourceLocation,
    },
    WhileLoopStmt {
        pred: Expr,
        body: Expr,
        position: SourceLocation,
    }
}

impl AST for Stmt {
    fn get_source_location(&self) -> SourceLocation {
        match self {
            Stmt::LetStmt { position, .. } => position.clone(),
            Stmt::StaticStmt { position, .. } => position.clone(),
            Stmt::FuncDeclaration { position, .. } => position.clone(),
            Stmt::ExprStmt(expr) => expr.get_source_location(),
            Stmt::IfElseStmt { position, .. } => position.clone(),
            Stmt::ForLoopStmt { position, .. } => position.clone(),
            Stmt::WhileLoopStmt { position, .. } => position.clone(),
        }
    }
}