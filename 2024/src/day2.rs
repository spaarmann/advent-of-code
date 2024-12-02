use itertools::Itertools;

use crate::util::SplitAndParse;

fn reports(input: &str) -> impl Iterator<Item = Vec<i64>> + '_ {
    input
        .lines()
        .map(|l| l.split_and_parse::<i64, _>(" ").collect_vec())
}

fn is_safe(report: &[i64]) -> bool {
    report
        .iter()
        .map_windows(|&[a, b]| a - b)
        .all(|d| d.signum() == (report[0] - report[1]).signum() && d.abs() >= 1 && d.abs() <= 3)
}

pub fn part1(input: &str) -> u64 {
    reports(input).filter(|report| is_safe(report)).count() as u64
}

pub fn part2(input: &str) -> u64 {
    reports(input)
        .filter(|report| {
            is_safe(report)
                || (0..report.len())
                    .map(|i| {
                        let mut dampened = report.clone();
                        dampened.remove(i);
                        dampened
                    })
                    .any(|r| is_safe(&r))
        })
        .count() as u64
}

#[cfg(test)]
mod tests {
    use super::*;

    const EXAMPLE_INPUT: &'static str = "7 6 4 2 1
1 2 7 8 9
9 7 6 2 1
1 3 2 4 5
8 6 4 4 1
1 3 6 7 9";

    #[test]
    fn p1_example() {
        assert_eq!(part1(EXAMPLE_INPUT), 2);
    }

    #[test]
    fn p1_input() {
        let input = std::fs::read_to_string("input/day2").expect("reading input file");
        assert_eq!(part1(&input), 421);
    }

    #[test]
    fn p2_example() {
        assert_eq!(part2(EXAMPLE_INPUT), 4);
    }

    #[test]
    fn p2_input() {
        let input = std::fs::read_to_string("input/day2").expect("reading input file");
        assert_eq!(part2(&input), todo!());
    }
}
