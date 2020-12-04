#![feature(str_split_once)]

use std::{env, fs};
mod day1;
mod day2;
mod day3;
mod day4;

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
        ("day2", 1) => println!("Result: {}", day2::part1(&input)),
        ("day2", 2) => println!("Result: {}", day2::part2(&input)),
        ("day3", 1) => println!("Result: {}", day3::part1(&input)),
        ("day3", 2) => println!("Result: {}", day3::part2(&input)),
        ("day4", 1) => println!("Result: {}", day4::part1(&input)),
        ("day4", 2) => println!("Result: {}", day4::part2(&input)),
        _ => panic!("day not implemented!"),
    };
}
