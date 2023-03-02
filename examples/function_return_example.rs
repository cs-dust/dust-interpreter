fn five() -> i64 {
    5
}

fn main() {
    let x = five();
    println("The value of x is 5: {}", x);

    let x = plus_one(x);
    println("The value of x is 6: {}", x);
}

fn plus_one(x: i64) -> i64 {
    x + 1
}