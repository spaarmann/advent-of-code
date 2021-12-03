#![feature(destructuring_assignment)]

use std::{env, fmt::Display, fs};

mod day1;
mod day2;
mod day3;
// MARK:MODULES

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
        ("day1", 1) => run(&input, day1::part1),
        ("day1", 2) => run(&input, day1::part2),
        ("day2", 1) => run(&input, day2::part1),
        ("day2", 2) => run(&input, day2::part2),
        ("day3", 1) => run(&input, day3::part1),
        ("day3", 2) => run(&input, day3::part2),
        // MARK:DAYS
        _ => panic!("day not implemented!"),
    }
}

fn run<T: Display, F: Fn(&str) -> T>(input: &str, f: F) {
    println!("Result: {}", f(input));
}
