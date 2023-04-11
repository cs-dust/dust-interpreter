// This program is a simple example to show variable declaration, types and printing messages to the console.
// Here we come across something called an owned string. In Rust, there are 2 implementations of strings -
// owned strings which can be mutated and string slices which are immutable.
// Dust’s implementation uses owned strings with variables & free strings as immutables by default.
// In this program, the variable c owns the String.
fn main() {
    let a = 2093;
    let b = true;
    let c = "help i am owned by c :(";
    let d = (); // Unassigned
    println("a is an integer: ", a);
    println("b is a boolean: ", b);
    println("c is an owned string: ", c);
    println("d is unitliteral: ", d);
    println("this is an immutable string");
}