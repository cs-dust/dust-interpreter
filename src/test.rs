#[cfg(test)]

mod test {

    use crate::interpreter;
    use crate::parser;
    use std::fs;

    const DEBUG_MODE: bool = false;

    #[test]
    fn test_empty_program() {
        let source = fs::read_to_string("examples/empty_program.rs").expect("Unable to read file");
        println!("Parsing...\n");
        let mut ast = parser::parse(&source).expect("Failed to parse given program");
        interpreter::run(&mut ast, DEBUG_MODE);
    }

    #[test]
    fn test_hello_world() {
        let source = fs::read_to_string("examples/hello_world.rs").expect("Unable to read file");
        println!("Parsing...\n");
        let mut ast = parser::parse(&source).expect("Failed to parse given program");
        interpreter::run(&mut ast, DEBUG_MODE);
    }

    #[test]
    fn test_simple_assignment() {
        let source =
            fs::read_to_string("examples/simple_assignment.rs").expect("Unable to read file");
        println!("Parsing...\n");
        let mut ast = parser::parse(&source).expect("Failed to parse given program");
        interpreter::run(&mut ast, DEBUG_MODE);
    }

    /**
    * Test: Primitive values and how they work
    **/
    #[test]
    fn test_primitive_values() {
        let source =
            fs::read_to_string("examples/primitive_value_example.rs").expect("Unable to read file");
        println!("Parsing...\n");
        let mut ast = parser::parse(&source).expect("Failed to parse given program");
        interpreter::run(&mut ast, DEBUG_MODE);
    }

    #[test]
    fn test_simple_unop() {
        let source = fs::read_to_string("examples/simple_unop.rs").expect("Unable to read file");
        println!("Parsing...\n");
        let mut ast = parser::parse(&source).expect("Failed to parse given program");
        interpreter::run(&mut ast, DEBUG_MODE);
    }

    #[test]
    fn test_simple_binop() {
        let source = fs::read_to_string("examples/simple_binop.rs").expect("Unable to read file");
        println!("Parsing...\n");
        let mut ast = parser::parse(&source).expect("Failed to parse given program");
        interpreter::run(&mut ast, DEBUG_MODE);
    }

    #[test]
    fn test_bool_binop() {
        let source =
            fs::read_to_string("examples/bool_types_with_binop.rs").expect("Unable to read file");
        println!("Parsing...\n");
        let mut ast = parser::parse(&source).expect("Failed to parse given program");
        interpreter::run(&mut ast, DEBUG_MODE);
    }

    #[test]
    fn test_numeric_binop() {
        let source = fs::read_to_string("examples/numeric_types_with_binop.rs")
            .expect("Unable to read file");
        println!("Parsing...\n");
        let mut ast = parser::parse(&source).expect("Failed to parse given program");
        interpreter::run(&mut ast, DEBUG_MODE);
    }

    #[test]
    fn test_block_expression() {
        let source = fs::read_to_string("examples/block_expression_example.rs")
            .expect("Unable to read file");
        println!("Parsing...\n");
        let mut ast = parser::parse(&source).expect("Failed to parse given program");
        interpreter::run(&mut ast, DEBUG_MODE);
    }

    /**
    * Test: Block expressions with return statements and primitive operations
    **/
    #[test]
    fn test_block_expression_with_primitive_op() {
        let source = fs::read_to_string("examples/block_expression_primitive_op.rs")
            .expect("Unable to read file");
        println!("Parsing...\n");
        let mut ast = parser::parse(&source).expect("Failed to parse given program");
        interpreter::run(&mut ast, DEBUG_MODE);
    }

    #[test]
    fn test_block_env() {
        let source =
            fs::read_to_string("examples/block_env_example.rs").expect("Unable to read file");
        println!("Parsing...\n");
        let mut ast = parser::parse(&source).expect("Failed to parse given program");
        interpreter::run(&mut ast, DEBUG_MODE);
    }

    #[test]
    fn test_function_return() {
        let source =
            fs::read_to_string("examples/function_return_example.rs").expect("Unable to read file");
        println!("Parsing...\n");
        let mut ast = parser::parse(&source).expect("Failed to parse given program");
        interpreter::run(&mut ast, DEBUG_MODE);
    }

    #[test]
    fn test_function_return_binop() {
        let source = fs::read_to_string("examples/function_return_addition.rs")
            .expect("Unable to read file");
        println!("Parsing...\n");
        let mut ast = parser::parse(&source).expect("Failed to parse given program");
        interpreter::run(&mut ast, DEBUG_MODE);
    }

    #[test]
    fn test_function_return_nested_vs_normal() {
        let source = fs::read_to_string("examples/function_application_nested_vs_normal.rs")
            .expect("Unable to read file");
        println!("Parsing...\n");
        let mut ast = parser::parse(&source).expect("Failed to parse given program");
        interpreter::run(&mut ast, DEBUG_MODE);
    }

    #[test]
    fn test_if_else_equality() {
        let source =
            fs::read_to_string("examples/if_else_example.rs").expect("Unable to read file");
        println!("Parsing...\n");
        let mut ast = parser::parse(&source).expect("Failed to parse given program");
        interpreter::run(&mut ast, DEBUG_MODE);
    }

    #[test]
    fn test_if_else_conjunction() {
        let source = fs::read_to_string("examples/if_else_conjunction_example.rs")
            .expect("Unable to read file");
        println!("Parsing...\n");
        let mut ast = parser::parse(&source).expect("Failed to parse given program");
        interpreter::run(&mut ast, DEBUG_MODE);
    }

    #[test]
    fn test_if_else_disjunction() {
        let source = fs::read_to_string("examples/if_else_disjunction_example.rs")
            .expect("Unable to read file");
        println!("Parsing...\n");
        let mut ast = parser::parse(&source).expect("Failed to parse given program");
        interpreter::run(&mut ast, DEBUG_MODE);
    }

    #[test]
    fn test_while_loop() {
        let source =
            fs::read_to_string("examples/while_loop_example.rs").expect("Unable to read file");
        println!("Parsing...\n");
        let mut ast = parser::parse(&source).expect("Failed to parse given program");
        interpreter::run(&mut ast, DEBUG_MODE);
    }

    /**
    * Test: Show loops in combination with other control flow statements like if else
    **/
    #[test]
    fn test_control_flow_fizzbuzz() {
        let source =
            fs::read_to_string("examples/control_flow_fizzbuzz.rs").expect("Unable to read file");
        println!("Parsing...\n");
        let mut ast = parser::parse(&source).expect("Failed to parse given program");
        interpreter::run(&mut ast, DEBUG_MODE);
    }

    #[test]
    fn test_string() {
        let source = fs::read_to_string("examples/string_example.rs").expect("Unable to read file");
        println!("Parsing...\n");
        let mut ast = parser::parse(&source).expect("Failed to parse given program");
        interpreter::run(&mut ast, DEBUG_MODE);
    }

    #[test]
    fn test_string_move() {
        let source =
            fs::read_to_string("examples/string_move_example.rs").expect("Unable to read file");
        println!("Parsing...\n");
        let mut ast = parser::parse(&source).expect("Failed to parse given program");
        interpreter::run(&mut ast, DEBUG_MODE);
    }

    #[test]
    fn test_string_concat_move1() {
        let source = fs::read_to_string("examples/string_concat_move_example1.rs")
            .expect("Unable to read file");
        println!("Parsing...\n");
        let mut ast = parser::parse(&source).expect("Failed to parse given program");
        interpreter::run(&mut ast, DEBUG_MODE);
    }

    #[test]
    fn test_string_concat_move2() {
        let source = fs::read_to_string("examples/string_concat_move_example2.rs")
            .expect("Unable to read file");
        println!("Parsing...\n");
        let mut ast = parser::parse(&source).expect("Failed to parse given program");
        interpreter::run(&mut ast, DEBUG_MODE);
    }

    #[test]
    fn test_string_overwrite() {
        let source =
            fs::read_to_string("examples/string_overwrite.rs").expect("Unable to read file");
        println!("Parsing...\n");
        let mut ast = parser::parse(&source).expect("Failed to parse given program");
        interpreter::run(&mut ast, DEBUG_MODE);
    }

    #[test]
    fn test_ownership_function() {
        let source = fs::read_to_string("examples/ownership_function_example.rs")
            .expect("Unable to read file");
        println!("Parsing...\n");
        let mut ast = parser::parse(&source).expect("Failed to parse given program");
        interpreter::run(&mut ast, DEBUG_MODE);
    }

    #[test]
    fn test_nested_function() {
        let source =
            fs::read_to_string("examples/nested_function.rs").expect("Unable to read file");
        println!("Parsing...\n");
        let mut ast = parser::parse(&source).expect("Failed to parse given program");
        interpreter::run(&mut ast, DEBUG_MODE);
    }
}
