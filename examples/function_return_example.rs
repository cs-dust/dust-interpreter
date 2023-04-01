fn five() -> i64 {
    5
}

fn main() {
    let x = five();
    println("The value of x is ", x);

    let x = plus_one(x);
    println("The value of x after plus one is ", x);
}

fn plus_one(x: i64) -> i64 {
    x + 1
}