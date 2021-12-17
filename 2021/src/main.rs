#![feature(destructuring_assignment)]

extern crate nalgebra as na;

use std::{env, fmt::Display, fs};

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
        ("day1", 1) => run(&input, aoc2021::day1::part1),
        ("day1", 2) => run(&input, aoc2021::day1::part2),
        ("day2", 1) => run(&input, aoc2021::day2::part1),
        ("day2", 2) => run(&input, aoc2021::day2::part2),
        ("day3", 1) => run(&input, aoc2021::day3::part1),
        ("day3", 2) => run(&input, aoc2021::day3::part2),
        ("day4", 1) => run(&input, aoc2021::day4::part1),
        ("day4", 2) => run(&input, aoc2021::day4::part2),
        ("day5", 1) => run(&input, aoc2021::day5::part1),
        ("day5", 2) => run(&input, aoc2021::day5::part2),
        ("day6", 1) => run(&input, aoc2021::day6::part1),
        ("day6", 2) => run(&input, aoc2021::day6::part2),
        ("day7", 1) => run(&input, aoc2021::day7::part1),
        ("day7", 2) => run(&input, aoc2021::day7::part2),
        ("day8", 1) => run(&input, aoc2021::day8::part1),
        ("day8", 2) => run(&input, aoc2021::day8::part2),
        ("day9", 1) => run(&input, aoc2021::day9::part1),
        ("day9", 2) => run(&input, aoc2021::day9::part2),
        ("day10", 1) => run(&input, aoc2021::day10::part1),
        ("day10", 2) => run(&input, aoc2021::day10::part2),
        ("day11", 1) => run(&input, aoc2021::day11::part1),
        ("day11", 2) => run(&input, aoc2021::day11::part2),
        ("day12", 1) => run(&input, aoc2021::day12::part1),
        ("day12", 2) => run(&input, aoc2021::day12::part2),
        ("day13", 1) => run(&input, aoc2021::day13::part1),
        ("day13", 2) => run(&input, aoc2021::day13::part2),
        ("day14", 1) => run(&input, aoc2021::day14::part1),
        ("day14", 2) => run(&input, aoc2021::day14::part2),
        ("day15", 1) => run(&input, aoc2021::day15::part1),
        ("day15", 2) => run(&input, aoc2021::day15::part2),
        ("day16", 1) => run(&input, aoc2021::day16::part1),
        ("day16", 2) => run(&input, aoc2021::day16::part2),
        ("day17", 1) => run(&input, aoc2021::day17::part1),
        ("day17", 2) => run(&input, aoc2021::day17::part2),
        // MARK:DAYS
        _ => panic!("day not implemented!"),
    }
}

fn run<T: Display, F: Fn(&str) -> T>(input: &str, f: F) {
    println!("Result: {}", f(input));
}
