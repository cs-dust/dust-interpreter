fn main() {
    fn nested_function(x: i64, y: i64) -> i64 {
        x + y
    }
    println("nested function adds 5 and 10 as", nested_function(five(), ten()));
    println("normal function adds 5 and 10 as", add(five(), ten()));
}

fn five() -> i64 {
    5
}

fn ten() -> i64 {
    10
}

fn add(x: i64, y: i64) -> i64 {
    x + y
}