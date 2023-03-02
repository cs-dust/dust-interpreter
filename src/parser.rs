// Initial: For testing, will change acc to our requirements
// oxido-parser
// Author: @chuabingquan for https://github.com/cs4215-seville/oxido-lang
//
#[allow(dead_code)]
pub mod ast;

use pest_consume::{match_nodes, Error, Parser};
use ast::{
    AST,
    Expr,
    Literal,
    DataType,
    PrimitiveOperation,
    UnaryOperator,
    BinaryOperator,
    SourceLocation,
    LifetimeParameter,
    FuncParameter,
    Stmt,
    Block,
    Sequence,
    SequenceStmt,
    VariadicOperator,
    PrimitiveOperator,
    // NullaryOperator,
};

#[derive(Parser)]
#[grammar = "oxido_grammar.pest"]
struct OxidoParser;

type Result<T> = std::result::Result<T, Error<Rule>>;
type Node<'i> = pest_consume::Node<'i, Rule, ()>;

#[pest_consume::parser]
impl OxidoParser {
    fn EOI(input: Node) -> Result<()> {
        Ok(())
    }
    fn program(input: Node) -> Result<Vec<Stmt>> {
        Ok(match_nodes!(input.into_children();
            [top_level_declarations(stmts).., _] => stmts.collect()
        ))
    }
    fn top_level_declarations(input: Node) -> Result<Stmt> {
        Ok(match_nodes!(input.into_children();
            [static_declaration(stmt)] => stmt,
            [function_declaration(stmt)] => stmt,
        ))
    }
    fn declaration(input: Node) -> Result<Stmt> {
        let (line, col) = input.as_span().start_pos().line_col();
        let position = SourceLocation { line, col };

        // Could probably be more concisely expressed by iterating through the input's children instead.
        let (name, is_mutable, annotation, value) =
            match_nodes!(input.children();
                [identifier(name)] =>
                    (name, false, None, None),
                [identifier(name), datatype(annotation)] =>
                    (name, false, Some(annotation), None),
                [identifier(name), expr(value)] =>
                    (name, false, None, Some(value)),
                [identifier(name), datatype(annotation), expr(value)] =>
                    (name, false, Some(annotation), Some(value)),
                [mutable_specifier(_m), identifier(name)] =>
                    (name, true, None, None),
                [mutable_specifier(_m), identifier(name), datatype(annotation)] =>
                    (name, true, Some(annotation), None),
                [mutable_specifier(_m), identifier(name), expr(value)] =>
                    (name, true, None, Some(value)),
                [mutable_specifier(_m), identifier(name), datatype(annotation), expr(value)] =>
                    (name, true, Some(annotation), Some(value)),
            );

        Ok(Stmt::LetStmt {
            name,
            is_mutable,
            annotation,
            value,
            position,
        })
    }
    fn static_declaration(input: Node) -> Result<Stmt> {
        let (line, col) = input.as_span().start_pos().line_col();
        let position = SourceLocation { line, col };

        Ok(match_nodes!(input.children();
            [identifier(identifier), datatype(annotation), expr(value)] =>
                Stmt::StaticStmt {
                    name: identifier,
                    is_mutable: false,
                    annotation,
                    value,
                    position,
                },
            [mutable_specifier(_m), identifier(identifier), datatype(annotation), expr(value)] =>
                Stmt::StaticStmt {
                    name: identifier,
                    is_mutable: true,
                    annotation,
                    value,
                    position,
                },
        ))
    }
    fn mutable_specifier(input: Node) -> Result<bool> {
        Ok(true)
    }
    fn datatype(input: Node) -> Result<DataType> {
        Ok(match input.as_str().trim() {
            "i64" => DataType::Int64,
            "bool" => DataType::Bool,
            "str" => DataType::Str,
            "String" => DataType::String,
            "()" => DataType::Unit,
            _ => match_nodes!(input.into_children();
                    [function_datatype(f)] => f,
                    [reference_datatype(d)] => d),
        })
    }
    fn reference_datatype(input: Node) -> Result<DataType> {
        let create_reference_type = |lifetime, is_mutable, datatype| match is_mutable {
            true => DataType::MutRef(lifetime, Box::from(datatype)),
            false => DataType::Ref(lifetime, Box::from(datatype)),
        };

        Ok(match_nodes!(input.into_children();
            [datatype(d)] =>
                create_reference_type(None, false, d),
            [lifetime_type_variable(l), datatype(d)] =>
                create_reference_type(Some(l), false, d),
            [mutable_specifier(_m), datatype(d)] =>
                create_reference_type(None, true, d),
            [lifetime_type_variable(l), mutable_specifier(_m), datatype(d)] =>
                create_reference_type(Some(l), true, d),
        ))
    }
    fn function_datatype(input: Node) -> Result<DataType> {
        Err(input.error("Function pointers are currently unsupported"))

        // Uncomment the following implementation when function pointers are supported.
        /* Ok(match_nodes!(input.into_children();
            [function_datatype_param_list(params), function_return_type(mut r)..] =>
                match r.next() {
                    None =>
                        DataType::Func(vec![], params, Box::from(DataType::Unit)),
                    Some(return_type) =>
                        DataType::Func(vec![], params, Box::from(return_type)),
                }
        )) */
    }
    fn function_datatype_param_list(input: Node) -> Result<Vec<DataType>> {
        Ok(match_nodes!(input.into_children();
            [datatype(d)..] => d.collect(),
        ))
    }
    fn block(input: Node) -> Result<Expr> {
        let process_stmts = |mut stmts: Sequence, last_expr| match last_expr {
            Some(expr) => match expr {
                return_expr@Expr::ReturnExpr(_, _) => {
                    stmts.push(SequenceStmt::Stmt(
                        Stmt::ExprStmt(return_expr),
                    ));
                    stmts
                },
                expr_to_return@_ => {
                    let return_expr_position = expr_to_return.get_source_location();
                    let return_expr = Expr::ReturnExpr(
                        Box::from(expr_to_return),
                        return_expr_position,
                    );
                    stmts.push(SequenceStmt::Stmt(
                        Stmt::ExprStmt(return_expr),
                    ));
                    stmts
                }
            },
            None => {
                // Temporary patch since source location isn't used.
                let position = SourceLocation {
                    line: 0,
                    col: 0,
                };
                let return_expr = Expr::ReturnExpr(
                    Box::from(Expr::LiteralExpr(
                        Literal::UnitLiteral,
                        position,
                    )),
                    position,
                );
                stmts.push(SequenceStmt::Stmt(
                    Stmt::ExprStmt(return_expr),
                ));
                stmts
            },
        };

        let (line, col) = input.as_span().start_pos().line_col();
        Ok(match_nodes!(input.into_children();
            [sequence(stmts), expr(mut last_expr)..] => Expr::BlockExpr(
                Box::from(Block {
                    statements: process_stmts(stmts, last_expr.next()),
                }),
                SourceLocation { line, col },
            )
        ))
    }
    fn sequence(input: Node) -> Result<Sequence> {
        input.children()
            .map(|node| match node.as_rule() {
                Rule::stmt => match OxidoParser::stmt(node) {
                    Ok(stmt) => Ok(SequenceStmt::Stmt(stmt)),
                    Err(msg) => Err(msg),
                },
                Rule::block => match OxidoParser::block(node) {
                    Ok(expr) => {
                        if let Expr::BlockExpr(block, _) = expr {
                            Ok(SequenceStmt::Block(*block))
                        } else {
                            Err(input.error("Sequence expects a block or a statement"))
                        }
                    },
                    Err(msg) => Err(msg),
                },
                _ => Err(input.error("Sequence expects a block or a statement"))
            })
            .collect()
    }
    fn stmt(input: Node) -> Result<Stmt> {
        Ok(match_nodes!(input.into_children();
            [declaration(stmt)] => stmt,
            [static_declaration(stmt)] => stmt,
            [function_declaration(stmt)] => stmt,
            [expr_stmt(stmt)] => stmt,
        ))
    }
    fn expr_stmt(input: Node) -> Result<Stmt> {
        Ok(match_nodes!(input.children();
            [expr(expr)] => Stmt::ExprStmt(expr),
        ))
    }
    fn expr(input: Node) -> Result<Expr> {
        Ok(match_nodes!(input.into_children();
            [assignment(expr)] => expr,
        ))
    }
    fn primary(input: Node) -> Result<Expr> {
        Ok(match_nodes!(input.into_children();
            [integer_literal(expr)] => expr,
            [string_literal(expr)] => expr,
            [boolean_literal(expr)] => expr,
            [unit_literal(expr)] => expr,
            [grouped_expr(expr)] => expr,
            [block(expr)] => expr,
            [return_val(expr)] => expr,
            [identifier(expr)] => expr,
        ))
    }
    fn grouped_expr(input: Node) -> Result<Expr> {
        Ok(match_nodes!(input.into_children();
            [expr(expr)] => expr,
        ))
    }
    fn assignment(input: Node) -> Result<Expr> {
        let (line, col) = input.as_span().start_pos().line_col();
        let position = SourceLocation { line, col };

        let is_valid_assignee = |assignee: &Expr| match assignee {
            Expr::IdentifierExpr(_, _) => true,
            Expr::PrimitiveOperationExpr(operation, _) => match *operation.clone() {
                PrimitiveOperation::UnaryOperation { operator, .. } => match operator {
                    UnaryOperator::Dereference => true,
                    _ => false,
                },
                _ => false,
            },
            _ => false,
        };

        let create_assignment_expr = |input: Node, assignee, value, position|
            match is_valid_assignee(&assignee) {
                true => Ok(Expr::AssignmentExpr {
                    assignee: Box::from(assignee),
                    value: Box::from(value),
                    position,
                }),
                false => Err(input.error("Expected assignee to be an identifier or a dereferenced expression")),
            };

        match_nodes!(input.children();
            [identifier(identifier), assignment(value)] =>
                create_assignment_expr(input, identifier, value, position),
            [unary(operation), assignment(value)] =>
                create_assignment_expr(input, operation, value, position),
            [disjunction(expr)] => Ok(expr),
        )
    }
    fn disjunction(input: Node) -> Result<Expr> {
        let create_binary_expr = |operator, first_operand, second_operand, src_location|
            Expr::PrimitiveOperationExpr(
                Box::from(PrimitiveOperation::BinaryOperation {
                    operator,
                    first_operand,
                    second_operand,
                }),
                src_location,
            );

        match_nodes!(input.children();
            [conjunction(initial_operand), conjunction(repetitions)..] => {
                let mut repetitions = repetitions.rev().peekable();
                match repetitions.next() {
                    Some(expr) => {
                        let mut second_operand = expr;

                        if repetitions.peek().is_none() {
                            let src_location = initial_operand.get_source_location();
                            Ok(create_binary_expr(
                                BinaryOperator::Or,
                                initial_operand,
                                second_operand,
                                src_location,
                            ))
                        } else {
                            for first_operand in repetitions {
                                let src_location = first_operand.get_source_location();
                                second_operand = create_binary_expr(
                                    BinaryOperator::Or,
                                    first_operand,
                                    second_operand,
                                    src_location,
                                );
                            }

                            let src_location = initial_operand.get_source_location();
                            Ok(create_binary_expr(
                                BinaryOperator::Or,
                                initial_operand,
                                second_operand,
                                src_location,
                            ))
                        }
                    },
                    None => Ok(initial_operand),
                }
            },
        )
    }
    fn conjunction(input: Node) -> Result<Expr> {
        let create_binary_expr = |operator, first_operand, second_operand, src_location|
            Expr::PrimitiveOperationExpr(
                Box::from(PrimitiveOperation::BinaryOperation {
                    operator,
                    first_operand,
                    second_operand,
                }),
                src_location,
            );

        match_nodes!(input.children();
            [equality(initial_operand), equality(repetitions)..] => {
                let mut repetitions = repetitions.rev().peekable();
                match repetitions.next() {
                    Some(expr) => {
                        let mut second_operand = expr;

                        if repetitions.peek().is_none() {
                            let src_location = initial_operand.get_source_location();
                            Ok(create_binary_expr(
                                BinaryOperator::And,
                                initial_operand,
                                second_operand,
                                src_location,
                            ))
                        } else {
                            for first_operand in repetitions {
                                let src_location = first_operand.get_source_location();
                                second_operand = create_binary_expr(
                                    BinaryOperator::And,
                                    first_operand,
                                    second_operand,
                                    src_location,
                                );
                            }

                            let src_location = initial_operand.get_source_location();
                            Ok(create_binary_expr(
                                BinaryOperator::And,
                                initial_operand,
                                second_operand,
                                src_location,
                            ))
                        }
                    },
                    None => Ok(initial_operand),
                }
            },
        )
    }
    fn equality(input: Node) -> Result<Expr> {
        let create_binary_expr = |operator, first_operand, second_operand, src_location|
            Expr::PrimitiveOperationExpr(
                Box::from(PrimitiveOperation::BinaryOperation {
                    operator,
                    first_operand,
                    second_operand,
                }),
                src_location,
            );

        match_nodes!(input.children();
            [comparison(initial_operand), equality_helper(repetitions)..] => {
                let mut repetitions = repetitions.rev().peekable();
                match repetitions.next() {
                    Some((op, expr)) => {
                        let mut current_op = op;
                        let mut second_operand = expr;

                        if repetitions.peek().is_none() {
                            let src_location = initial_operand.get_source_location();
                            Ok(create_binary_expr(
                                current_op,
                                initial_operand,
                                second_operand,
                                src_location,
                            ))
                        } else {
                            for (op, first_operand) in repetitions {
                                let src_location = first_operand.get_source_location();
                                second_operand = create_binary_expr(
                                    current_op,
                                    first_operand,
                                    second_operand,
                                    src_location,
                                );
                                current_op = op;
                            }

                            let src_location = initial_operand.get_source_location();
                            Ok(create_binary_expr(
                                current_op,
                                initial_operand,
                                second_operand,
                                src_location,
                            ))
                        }
                    },
                    None => Ok(initial_operand),
                }
            },
        )
    }
    fn equality_operator(input: Node) -> Result<BinaryOperator> {
        match input.as_str() {
            "!=" => Ok(BinaryOperator::NotEqual),
            "==" => Ok(BinaryOperator::Equal),
            unsupported_op@_ => {
                let msg = format!("The \"{}\" operator is unsupported", unsupported_op);
                Err(input.error(msg))
            }
        }
    }
    fn equality_helper(input: Node) -> Result<(BinaryOperator, Expr)> {
        Ok(match_nodes!(input.into_children();
            [equality_operator(op), comparison(expr)] => (op, expr),
        ))
    }
    fn comparison(input: Node) -> Result<Expr> {
        let create_binary_expr = |operator, first_operand, second_operand, src_location|
            Expr::PrimitiveOperationExpr(
                Box::from(PrimitiveOperation::BinaryOperation {
                    operator,
                    first_operand,
                    second_operand,
                }),
                src_location,
            );

        match_nodes!(input.children();
            [term(initial_operand), comparison_helper(repetitions)..] => {
                let mut repetitions = repetitions.rev().peekable();
                match repetitions.next() {
                    Some((op, expr)) => {
                        let mut current_op = op;
                        let mut second_operand = expr;

                        if repetitions.peek().is_none() {
                            let src_location = initial_operand.get_source_location();
                            Ok(create_binary_expr(
                                current_op,
                                initial_operand,
                                second_operand,
                                src_location,
                            ))
                        } else {
                            for (op, first_operand) in repetitions {
                                let src_location = first_operand.get_source_location();
                                second_operand = create_binary_expr(
                                    current_op,
                                    first_operand,
                                    second_operand,
                                    src_location,
                                );
                                current_op = op;
                            }

                            let src_location = initial_operand.get_source_location();
                            Ok(create_binary_expr(
                                current_op,
                                initial_operand,
                                second_operand,
                                src_location,
                            ))
                        }
                    },
                    None => Ok(initial_operand),
                }
            },
        )
    }
    fn comparison_operator(input: Node) -> Result<BinaryOperator> {
        match input.as_str() {
            ">" => Ok(BinaryOperator::Greater),
            ">=" => Ok(BinaryOperator::GreaterOrEqual),
            "<" => Ok(BinaryOperator::Less),
            "<=" => Ok(BinaryOperator::LessOrEqual),
            unsupported_op@_ => {
                let msg = format!("The \"{}\" operator is unsupported", unsupported_op);
                Err(input.error(msg))
            }
        }
    }
    fn comparison_helper(input: Node) -> Result<(BinaryOperator, Expr)> {
        Ok(match_nodes!(input.into_children();
            [comparison_operator(op), term(expr)] => (op, expr),
        ))
    }
    fn term(input: Node) -> Result<Expr> {
        let create_binary_expr = |operator, first_operand, second_operand, src_location|
            Expr::PrimitiveOperationExpr(
                Box::from(PrimitiveOperation::BinaryOperation {
                    operator,
                    first_operand,
                    second_operand,
                }),
                src_location,
            );

        match_nodes!(input.children();
            [factor(initial_operand), term_helper(repetitions)..] => {
                let mut repetitions = repetitions.rev().peekable();
                match repetitions.next() {
                    Some((op, expr)) => {
                        let mut current_op = op;
                        let mut second_operand = expr;

                        if repetitions.peek().is_none() {
                            let src_location = initial_operand.get_source_location();
                            Ok(create_binary_expr(
                                current_op,
                                initial_operand,
                                second_operand,
                                src_location,
                            ))
                        } else {
                            for (op, first_operand) in repetitions {
                                let src_location = first_operand.get_source_location();
                                second_operand = create_binary_expr(
                                    current_op,
                                    first_operand,
                                    second_operand,
                                    src_location,
                                );
                                current_op = op;
                            }

                            let src_location = initial_operand.get_source_location();
                            Ok(create_binary_expr(
                                current_op,
                                initial_operand,
                                second_operand,
                                src_location,
                            ))
                        }
                    },
                    None => Ok(initial_operand),
                }
            },
        )
    }
    fn term_operator(input: Node) -> Result<BinaryOperator> {
        match input.as_str() {
            "-" => Ok(BinaryOperator::Minus),
            "+" => Ok(BinaryOperator::Plus),
            unsupported_op@_ => {
                let msg = format!("The \"{}\" operator is unsupported", unsupported_op);
                Err(input.error(msg))
            }
        }
    }
    fn term_helper(input: Node) -> Result<(BinaryOperator, Expr)> {
        Ok(match_nodes!(input.into_children();
            [term_operator(op), factor(expr)] => (op, expr),
        ))
    }
    fn factor(input: Node) -> Result<Expr> {
        let create_binary_expr = |operator, first_operand, second_operand, src_location|
            Expr::PrimitiveOperationExpr(
                Box::from(PrimitiveOperation::BinaryOperation {
                    operator,
                    first_operand,
                    second_operand,
                }),
                src_location,
            );

        match_nodes!(input.children();
            [unary(initial_operand), factor_helper(repetitions)..] => {
                let mut repetitions = repetitions.rev().peekable();
                match repetitions.next() {
                    Some((op, expr)) => {
                        let mut current_op = op;
                        let mut second_operand = expr;

                        if repetitions.peek().is_none() {
                            let src_location = initial_operand.get_source_location();
                            Ok(create_binary_expr(
                                current_op,
                                initial_operand,
                                second_operand,
                                src_location,
                            ))
                        } else {
                            for (op, first_operand) in repetitions {
                                let src_location = first_operand.get_source_location();
                                second_operand = create_binary_expr(
                                    current_op,
                                    first_operand,
                                    second_operand,
                                    src_location,
                                );
                                current_op = op;
                            }

                            let src_location = initial_operand.get_source_location();
                            Ok(create_binary_expr(
                                current_op,
                                initial_operand,
                                second_operand,
                                src_location,
                            ))
                        }
                    },
                    None => Ok(initial_operand),
                }
            },
        )
    }
    fn factor_operator(input: Node) -> Result<BinaryOperator> {
        match input.as_str() {
            "/" => Ok(BinaryOperator::Divide),
            "*" => Ok(BinaryOperator::Times),
            unsupported_op@_ => {
                let msg = format!("The \"{}\" operator is unsupported", unsupported_op);
                Err(input.error(msg))
            }
        }
    }
    fn factor_helper(input: Node) -> Result<(BinaryOperator, Expr)> {
        Ok(match_nodes!(input.into_children();
            [factor_operator(op), unary(expr)] => (op, expr),
        ))
    }
    fn unary(input: Node) -> Result<Expr> {
        let create_unary_expr = |operator, operand, line, col| Expr::PrimitiveOperationExpr(
            Box::from(PrimitiveOperation::UnaryOperation {
                operator,
                operand,
            }),
            SourceLocation { line, col },
        );

        let (line, col) = input.as_span().start_pos().line_col();

        Ok(match_nodes!(input.into_children();
            [unary_operator(op), unary(expr)]
                => create_unary_expr(op, expr, line, col),
            [function_app(expr)] => expr,
        ))
    }
    fn unary_operator(input: Node) -> Result<UnaryOperator> {
        match input.as_str() {
            "!" => Ok(UnaryOperator::Not),
            "-" => Ok(UnaryOperator::UnaryMinus),
            "&mut " => Ok(UnaryOperator::MutableBorrow),
            "&" => Ok(UnaryOperator::ImmutableBorrow),
            "*" => Ok(UnaryOperator::Dereference),
            unsupported_op@_ => {
                let msg = format!("The \"{}\" operator is unsupported", unsupported_op);
                Err(input.error(msg))
            }
        }
    }
    fn return_val(input: Node) -> Result<Expr> {
        let (line, col) = input.as_span().start_pos().line_col();
        Ok(match_nodes!(input.into_children();
            [expr(expr)] => Expr::ReturnExpr(
                Box::from(expr),
                SourceLocation { line, col },
            ),
        ))
    }
    fn identifier(input: Node) -> Result<Expr> {
        let (line, col) = input.as_span().start_pos().line_col();
        Ok(Expr::IdentifierExpr(
            String::from(input.as_str().trim()),
            SourceLocation { line, col }
        ))
    }
    fn function_declaration(input: Node) -> Result<Stmt> {
        let process_block_expr = |block_expr| match block_expr {
            Expr::BlockExpr(block, _) => Ok(*block),
            _ => Err("Body (block expression) is expected in a function declaration")
        };

        let (line, col) = input.as_span().start_pos().line_col();
        let position = SourceLocation { line, col };

        let create_func_decl =
            |input: Node, name, lifetime_parameters, parameters, return_type: Option<DataType>, block_expr| {
                let block = process_block_expr(block_expr).map_err(|e| input.error(e))?;
                Ok(Stmt::FuncDeclaration {
                    name,
                    lifetime_parameters,
                    parameters,
                    return_type: return_type.unwrap_or(DataType::Unit),
                    body: block,
                    position,
                })
            };

        match_nodes!(input.children();
            [
                identifier(name),
                function_param_list(parameters),
                function_return_type(mut return_type)..,
                block(block_expr),
            ] =>
                create_func_decl(input, name, vec![], parameters, return_type.next(), block_expr),
            [
                identifier(name),
                lifetime_param_list(lifetime_parameters),
                function_param_list(parameters),
                function_return_type(mut return_type)..,
                block(block_expr),
            ] =>
                create_func_decl(input, name, lifetime_parameters, parameters, return_type.next(), block_expr),
        )
    }
    fn function_return_type(input: Node) -> Result<DataType> {
        Ok(match_nodes!(input.into_children();
            [datatype(d)] => d,
        ))
    }
    fn lifetime_param_list(input: Node) -> Result<Vec<LifetimeParameter>> {
        input.into_children()
            .map(OxidoParser::lifetime_type_variable)
            .collect()
    }
    fn lifetime_type_variable(input: Node) -> Result<LifetimeParameter> {
        Ok(String::from(input.as_str()))
    }
    fn function_param_list(input: Node) -> Result<Vec<FuncParameter>> {
        input.into_children()
            .map(OxidoParser::function_param)
            .collect()
    }
    fn function_param(input: Node) -> Result<FuncParameter> {
        Ok(match_nodes!(input.children();
            [identifier(name), datatype(param_type)] =>
                (name, param_type),
        ))
    }
    fn function_app(input: Node) -> Result<Expr> {
        // Since parser doesn't allow infixed functions to be called in a prefixed manner,
        // we only need to handle for prefixed primitive functions.
        let get_prefixed_primitive_operator = |callee: &Expr| match callee {
            Expr::IdentifierExpr(name, _) => match name.as_str() {
                "string_from" =>
                    Some(PrimitiveOperator::Unary(UnaryOperator::StringFrom)),
                "drop" =>
                    Some(PrimitiveOperator::Unary(UnaryOperator::Drop)),
                "len" =>
                    Some(PrimitiveOperator::Unary(UnaryOperator::Len)),
                "as_str" =>
                    Some(PrimitiveOperator::Unary(UnaryOperator::AsStr)),
                "push_str" =>
                    Some(PrimitiveOperator::Unary(UnaryOperator::PushStr)),
                // "main" =>
                //     Some(PrimitiveOperator::Nullary(NullaryOperator::Main)),
                "println" =>
                    Some(PrimitiveOperator::VariadicOperator(VariadicOperator::Println)),
                _ => None,
            },
            _ => None,
        };

        let (line, col) = input.as_span().start_pos().line_col();
        Ok(match_nodes!(input.into_children();
            [primary(expr)] => expr,
            [primary(callee), function_arg_list(arguments)] => Expr::ApplicationExpr {
                is_primitive: get_prefixed_primitive_operator(&callee),
                callee: Box::from(callee),
                arguments,
                position: SourceLocation { line, col },
            },
        ))
    }
    fn function_arg_list(input: Node) -> Result<Vec<Expr>> {
        input.into_children()
            .map(OxidoParser::expr)
            .collect()
    }
    fn boolean_literal(input: Node) -> Result<Expr> {
        input.as_str()
            .trim()
            .parse::<bool>()
            .map(|b| -> Expr {
                let (line, col) = input.as_span().start_pos().line_col();
                Expr::LiteralExpr(
                    Literal::BoolLiteral(b),
                    SourceLocation { line, col }
                )
            })
            .map_err(|e| input.error(e))
    }
    fn integer_literal(input: Node) -> Result<Expr> {
        input.as_str()
            .trim()
            .parse::<i64>()
            .map(|i| -> Expr {
                let (line, col) = input.as_span().start_pos().line_col();
                Expr::LiteralExpr(
                    Literal::IntLiteral(i),
                    SourceLocation { line, col }
                )
            })
            .map_err(|e| input.error(e))
    }
    fn string_literal(input: Node) -> Result<Expr> {
        let (line, col) = input.as_span().start_pos().line_col();
        let s = input.into_children().as_pairs().as_str();
        let str_expr = Expr::LiteralExpr(
            Literal::StringLiteral(String::from(s)),
            SourceLocation { line, col }
        );
        Ok(str_expr)
    }
    fn unit_literal(input: Node) -> Result<Expr> {
        let (line, col) = input.as_span().start_pos().line_col();
        Ok(Expr::LiteralExpr(
            Literal::UnitLiteral,
            SourceLocation { line, col },
        ))
    }
}

pub fn parse(program: &str) -> Result<Vec<Stmt>> {
    let inputs = OxidoParser::parse(Rule::program, &program)?;
    OxidoParser::program(inputs.single()?)
}

#[cfg(test)]
mod tests {
    use super::parse;
    use walkdir::{WalkDir, DirEntry};
    use std::fs;

    type GetFilesPredicate = fn(&DirEntry) -> bool;

    fn get_files_from(path: &str, predicate: GetFilesPredicate) -> Vec<DirEntry> {
        WalkDir::new(path)
            .follow_links(true)
            .into_iter()
            .filter_map(|e| e.ok())
            .filter(predicate)
            .collect::<Vec<DirEntry>>()
    }

    #[test]
    fn test_parse() {
        let test_dir = "parse_examples";
        let valid_program_entries = get_files_from(test_dir, |e| {
            let file_name = e.file_name().to_string_lossy();
            file_name.ends_with(".rs") && file_name != "statement_parse_error.rs"
        });

        assert!(valid_program_entries.len() > 0);

        valid_program_entries
            .iter()
            .for_each(|file| {
                let program = fs::read_to_string(file.path()).expect("Unable to read valid test program");
                assert!(parse(&program).is_ok(), "Failed to parse syntatically valid program: {:#?}", file.path());
            });

        let invalid_programs_entries = get_files_from(test_dir, |e|
            e.file_name().to_string_lossy() == "statement_parse_error.rs");

        assert!(invalid_programs_entries.len() > 0);

        invalid_programs_entries
            .iter()
            .for_each(|file| {
                let program = fs::read_to_string(file.path()).expect("Unable to read invalid test program");
                assert!(parse(&program).is_err(), "Failed to reject syntatically invalid program: {:#?}", file.path());
            });

    }
}