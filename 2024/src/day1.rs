use std::collections::HashMap;

use crate::util::SplitAndParse;

fn parse(input: &str) -> (Vec<u64>, Vec<u64>) {
    input
        .lines()
        .map(|l| l.split_once_and_parse::<u64, _>("   "))
        .unzip()
}

pub fn part1(input: &str) -> u64 {
    let (mut left, mut right) = parse(input);
    left.sort();
    right.sort();

    left.iter()
        .zip(right.iter())
        .map(|(&l, &r)| l.abs_diff(r))
        .sum()
}

pub fn part2(input: &str) -> u64 {
    let (left, right) = parse(input);

    let mut occurences = HashMap::new();
    for r in right {
        *(occurences.entry(r).or_insert(0)) += 1;
    }

    left.iter()
        .map(|l| l * occurences.get(l).unwrap_or(&0))
        .sum()
}

#[cfg(test)]
mod tests {
    use super::*;

    const EXAMPLE_INPUT: &'static str = "3   4
4   3
2   5
1   3
3   9
3   3";

    #[test]
    fn p1_example() {
        assert_eq!(part1(EXAMPLE_INPUT), 11);
    }

    #[test]
    fn p1_input() {
        let input = std::fs::read_to_string("input/day1").expect("reading input file");
        assert_eq!(part1(&input), 1258579);
    }

    #[test]
    fn p2_example() {
        assert_eq!(part2(EXAMPLE_INPUT), 31);
    }

    #[test]
    fn p2_input() {
        let input = std::fs::read_to_string("input/day1").expect("reading input file");
        assert_eq!(part2(&input), 23981443);
    }
}
