use std::{num::ParseIntError, str::FromStr};

#[derive(Copy, Clone, Debug)]
struct Assignment {
    start: u64,
    end: u64,
}

impl FromStr for Assignment {
    type Err = ParseIntError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let (start, end) = s.split_once('-').unwrap();
        Ok(Assignment {
            start: start.parse()?,
            end: end.parse()?,
        })
    }
}

impl Assignment {
    fn contains(self, other: Self) -> bool {
        self.start <= other.start && self.end >= other.end
    }

    fn overlaps(self, other: Self) -> bool {
        self.start <= other.end && self.end >= other.start
    }
}

fn parse(input: &str) -> impl Iterator<Item = (Assignment, Assignment)> + '_ {
    input.lines().map(|line| {
        let (a, b) = line.split_once(',').unwrap();
        (a.parse().unwrap(), b.parse().unwrap())
    })
}

pub fn part1(input: &str) -> u64 {
    parse(input)
        .filter(|&(a, b)| a.contains(b) || b.contains(a))
        .count() as u64
}

pub fn part2(input: &str) -> u64 {
    parse(input).filter(|&(a, b)| a.overlaps(b)).count() as u64
}

#[cfg(test)]
mod tests {
    use super::*;

    const EXAMPLE_INPUT: &'static str = "2-4,6-8
2-3,4-5
5-7,7-9
2-8,3-7
6-6,4-6
2-6,4-8";

    #[test]
    fn p1_example() {
        assert_eq!(part1(EXAMPLE_INPUT), 2);
    }

    #[test]
    fn p1_input() {
        let input = std::fs::read_to_string("input/day4").expect("reading input file");
        assert_eq!(part1(&input), 305);
    }

    #[test]
    fn p2_example() {
        assert_eq!(part2(EXAMPLE_INPUT), 4);
    }

    #[test]
    fn p2_input() {
        let input = std::fs::read_to_string("input/day4").expect("reading input file");
        assert_eq!(part2(&input), 811);
    }
}
