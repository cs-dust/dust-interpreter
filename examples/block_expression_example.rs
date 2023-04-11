// We will see how the values on the heap are freed when we go out of scope of the block expression.
fn main() {
    let y = {
        let x = 3;
        x + 1
    };

    println("The value of y is ", y);
}