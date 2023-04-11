//This program demonstrates Dust’s support for code blocks and how expressions can be evaluated within these blocks,
// returning the value of the last expression.
// Additionally, the program also shows how semicolons can be used to suppress the output of expressions.
fn main() {
    let x = 5;

    let y = {
        let x_squared = x * x;
        let x_cube = x_squared * x;

        // This expression will be assigned to `y`
        x_cube + x_squared + x
    };

    let z = {
        // The semicolon suppresses this expression and `()` is assigned to `z`
        2 * x;
    };

    println("x is ", x);
    println("y is ", y);
    println("z is ", z);
}