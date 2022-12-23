use std::ops::Range;

use itertools::Itertools;
use lazy_static::lazy_static;
use regex::Regex;

extern crate nalgebra as na;

type Vec2 = na::Vector2<i64>;

fn parse(input: &str) -> impl Iterator<Item = (Vec2, Vec2)> + '_ {
    lazy_static! {
        static ref RE: Regex = Regex::new(
            r"Sensor at x=(-?\d+), y=(-?\d+): closest beacon is at x=(-?\d+), y=(-?\d+)"
        )
        .unwrap();
    }

    input.lines().map(|l| {
        let captures = RE.captures(l).unwrap();
        (
            Vec2::new(
                captures.get(1).unwrap().as_str().parse().unwrap(),
                captures.get(2).unwrap().as_str().parse().unwrap(),
            ),
            Vec2::new(
                captures.get(3).unwrap().as_str().parse().unwrap(),
                captures.get(4).unwrap().as_str().parse().unwrap(),
            ),
        )
    })
}

fn calc_part1(input: &str, row: i64) -> u64 {
    let ranges = parse(input)
        .map(|(sensor, beacon)| {
            let manhattan_dist = (beacon - sensor).abs().sum();
            let to_row = (sensor.y - row).abs();
            let remaining_dist = manhattan_dist - to_row;
            (sensor.x - remaining_dist)..(sensor.x + remaining_dist)
        })
        .sorted_by(|a, b| a.start.cmp(&b.start).then(a.end.cmp(&b.end)));

    let mut union_ranges = Vec::new();
    for range in ranges {
        if union_ranges
            .last()
            .map(|r: &Range<i64>| r.end >= range.start - 1)
            .unwrap_or(false)
        {
            let last = union_ranges.last_mut().unwrap();
            last.end = last.end.max(range.end);
        } else {
            union_ranges.push(range);
        }
    }

    union_ranges
        .into_iter()
        .map(|r| r.end - r.start)
        .sum::<i64>() as u64
}

pub fn part1_example(input: &str) -> u64 {
    calc_part1(input, 10)
}

pub fn part1(input: &str) -> u64 {
    calc_part1(input, 2000000)
}

fn calc_part2(input: &str, max_coord: i64) -> u64 {
    let input = parse(input).collect_vec();

    for row in 0..max_coord {
        let ranges = input
            .iter()
            .map(|(sensor, beacon)| {
                let manhattan_dist = (beacon - sensor).abs().sum();
                let to_row = (sensor.y - row).abs();
                let remaining_dist = manhattan_dist - to_row;
                (sensor.x - remaining_dist)..(sensor.x + remaining_dist)
            })
            .map(|r| r.start.clamp(0, max_coord)..r.end.clamp(0, max_coord))
            .sorted_by(|a, b| a.start.cmp(&b.start).then(a.end.cmp(&b.end)));

        let mut union_ranges = Vec::new();
        for range in ranges {
            if union_ranges
                .last()
                .map(|r: &Range<i64>| r.end >= range.start - 1)
                .unwrap_or(false)
            {
                let last = union_ranges.last_mut().unwrap();
                last.end = last.end.max(range.end);
            } else {
                union_ranges.push(range);
            }
        }

        if union_ranges.len() != 1 {
            return ((union_ranges[0].end + 1) * 4000000 + row) as u64;
        }
    }

    panic!("did not find any empty spot")
}

pub fn part2_example(input: &str) -> u64 {
    calc_part2(input, 20)
}

pub fn part2(input: &str) -> u64 {
    calc_part2(input, 4000000)
}

#[cfg(test)]
mod tests {
    use super::*;

    const EXAMPLE_INPUT: &'static str = "Sensor at x=2, y=18: closest beacon is at x=-2, y=15
Sensor at x=9, y=16: closest beacon is at x=10, y=16
Sensor at x=13, y=2: closest beacon is at x=15, y=3
Sensor at x=12, y=14: closest beacon is at x=10, y=16
Sensor at x=10, y=20: closest beacon is at x=10, y=16
Sensor at x=14, y=17: closest beacon is at x=10, y=16
Sensor at x=8, y=7: closest beacon is at x=2, y=10
Sensor at x=2, y=0: closest beacon is at x=2, y=10
Sensor at x=0, y=11: closest beacon is at x=2, y=10
Sensor at x=20, y=14: closest beacon is at x=25, y=17
Sensor at x=17, y=20: closest beacon is at x=21, y=22
Sensor at x=16, y=7: closest beacon is at x=15, y=3
Sensor at x=14, y=3: closest beacon is at x=15, y=3
Sensor at x=20, y=1: closest beacon is at x=15, y=3";

    #[test]
    fn p1_example() {
        assert_eq!(part1_example(EXAMPLE_INPUT), 26);
    }

    #[test]
    fn p1_input() {
        let input = std::fs::read_to_string("input/day15").expect("reading input file");
        assert_eq!(part1(&input), 5716881);
    }

    #[test]
    fn p2_example() {
        assert_eq!(part2_example(EXAMPLE_INPUT), 56000011);
    }

    #[test]
    #[ignore = "too slow"] // TODO
    fn p2_input() {
        let input = std::fs::read_to_string("input/day15").expect("reading input file");
        assert_eq!(part2(&input), 10852583132904);
    }
}
