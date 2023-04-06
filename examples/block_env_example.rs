fn main() {
    let x = 0;
    {
        let x = 10;
        println("Value of x in the block is ", x);
    }
    println("Value of x outside the block is ", x);
}