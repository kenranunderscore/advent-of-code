fn main() {
    let input = include_str!("../inputs/day1.txt");

    // hacky part 1
    let mut lefts = vec![];
    let mut rights = vec![];
    for line in input.lines() {
        let left: i32 = line
            .split_ascii_whitespace()
            .nth(0)
            .unwrap()
            .to_string()
            .parse()
            .unwrap();
        lefts.push(left);
        let right: i32 = line
            .split_ascii_whitespace()
            .nth(1)
            .unwrap()
            .to_string()
            .parse()
            .unwrap();
        rights.push(right);
    }
    lefts.sort();
    rights.sort();
    let sum: i32 = lefts
        .iter()
        .zip(rights.iter())
        .map(|(l, r)| (l - r).abs())
        .sum();
    println!("sum: {sum}");
}
