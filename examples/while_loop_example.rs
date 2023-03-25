fn main() {
    let mut x = 3;
    while x > 0 {
        println!("decrmenting {}", x);
        x = x - 1;
    }

    while x < 3 {
        println!("incrmenting {}", x);
        x += 1;
    }
}