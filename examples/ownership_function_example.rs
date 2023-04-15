fn main() {
    let number = 1;
    makes_copy(number);
    println("integer number after function call ", number);

    let x = "hello";
    takes_ownership(x);
    println("string x after function call: ", x);
}

fn takes_ownership(s: String) {
    println("Taken ownership of ", s);
}

fn makes_copy(i: i64) {
    println("Made a copy of ", i);
}