use std::{env, fs};
mod day1;

fn main() {
    let day = env::args()
        .nth(1)
        .expect("Pass day to execute as first argument");
    let part = env::args()
        .nth(2)
        .map(|s| s.parse::<u8>().expect("part must be a valid integer"))
        .unwrap_or(1);
    let input = fs::read_to_string(format!("input/{}", day)).expect("failed to read input file");

    match (day.as_str(), part) {
        ("day1", 1) => println!("Result: {}", day1::part1(&input)),
        ("day1", 2) => println!("Result: {}", day1::part2(&input)),
        _ => panic!("day not implemented!"),
    };
}
