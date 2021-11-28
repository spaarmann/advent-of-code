#![feature(str_split_once)]
#![feature(destructuring_assignment)]
#![feature(min_const_generics)]
#![feature(unsigned_abs)]

use std::{env, fs};
mod day1;
mod day10;
mod day11;
mod day12;
mod day13;
mod day14;
mod day15;
mod day16;
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
        ("day10", 1) => println!("Result: {}", day10::part1(&input)),
        ("day10", 2) => println!("Result: {}", day10::part2(&input)),
        ("day11", 1) => println!("Result: {}", day11::part1(&input)),
        ("day11", 2) => println!("Result: {}", day11::part2(&input)),
        ("day12", 1) => println!("Result: {}", day12::part1(&input)),
        ("day12", 2) => println!("Result: {}", day12::part2(&input)),
        ("day13", 1) => println!("Result: {}", day13::part1(&input)),
        ("day13", 2) => println!("Result: {}", day13::part2(&input)),
        ("day14", 1) => println!("Result: {}", day14::part1(&input)),
        ("day14", 2) => println!("Result: {}", day14::part2(&input)),
        ("day15", 1) => println!("Result: {}", day15::part1(&input)),
        ("day15", 2) => println!("Result: {}", day15::part2(&input)),
        ("day16", 1) => println!("Result: {}", day16::part1(&input)),
        ("day16", 2) => println!("Result: {}", day16::part2(&input)),
        _ => panic!("day not implemented!"),
    };
}
