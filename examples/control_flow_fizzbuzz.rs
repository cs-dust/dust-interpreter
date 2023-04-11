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