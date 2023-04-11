// This program demonstrates Dust's support for loops and conditional statements.
// It also shows how variables can be declared and reassigned.
// In our system, all variables are mutable by default, something that is different from Rust.
fn main() {
    // A counter variable
    let mut n = 1;

    // Loop while `n` is less than 101
    while n < 10 {
        if n == 5 || n == 7 {
            println("fizz");
        } else {
            println("buzz");
        }
        // Increment counter
        n = n + 1;
    }
}