fn main() {
    let x = 0;
    {
        let x = 10;
        println("{}", x);
    }
    println("{}", x);
}