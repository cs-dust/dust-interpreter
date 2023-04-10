fn main() {
    let a = "hello";
    let b = " world";
    let c = a + b;
    println("a should have moved: ", a);
    println("b should remain since it was borrowed", b);
    println("c is the new concatenated string", c);
}