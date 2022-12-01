use itertools::Itertools;

fn parse(input: &str) -> impl Iterator<Item = u64> + '_ {
    input
        .split("\n\n")
        .map(|elf| elf.lines().map(|l| l.parse::<u64>().unwrap()).sum())
}

pub fn part1(input: &str) -> u64 {
    parse(input).max().unwrap()
}

pub fn part2(input: &str) -> u64 {
    parse(input).sorted().rev().take(3).sum()
}

#[cfg(test)]
mod tests {
    use super::*;

    const EXAMPLE_INPUT: &'static str = "1000
2000
3000

4000

5000
6000

7000
8000
9000

10000";

    #[test]
    fn p1_example() {
        assert_eq!(part1(EXAMPLE_INPUT), 24000);
    }

    #[test]
    fn p1_input() {
        let input = std::fs::read_to_string("input/day1").expect("reading input file");
        assert_eq!(part1(&input), 69883);
    }

    #[test]
    fn p2_example() {
        assert_eq!(part2(EXAMPLE_INPUT), 45000);
    }

    #[test]
    fn p2_input() {
        let input = std::fs::read_to_string("input/day1").expect("reading input file");
        assert_eq!(part2(&input), todo!());
    }
}
