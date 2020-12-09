#![feature(str_split_once)]
#![feature(destructuring_assignment)]
#![feature(min_const_generics)]

use std::{env, fs};
mod day1;
mod day2;
mod day3;
mod day4;
mod day5;
mod day6;
mod day7;
mod day8;
mod day9;

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
        ("day5", 1) => println!("Result: {}", day5::part1(&input)),
        ("day5", 2) => println!("Result: {}", day5::part2(&input)),
        ("day6", 1) => println!("Result: {}", day6::part1(&input)),
        ("day6", 2) => println!("Result: {}", day6::part2(&input)),
        ("day7", 1) => println!("Result: {}", day7::part1(&input)),
        ("day7", 2) => println!("Result: {}", day7::part2(&input)),
        ("day8", 1) => println!("Result: {}", day8::part1(&input)),
        ("day8", 2) => println!("Result: {}", day8::part2(&input)),
        ("day9", 1) => println!("Result: {}", day9::part1(&input)),
        ("day9", 2) => println!("Result: {}", day9::part2(&input)),
        _ => panic!("day not implemented!"),
    };
}
