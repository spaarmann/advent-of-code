use itertools::Itertools;

pub fn part1(input: &str) -> u64 {
    input
        .lines()
        .map(|l| l.parse::<u64>().unwrap())
        .tuple_windows()
        .map(|(a, b)| (b > a) as u64)
        .sum()
}

pub fn part2(input: &str) -> u64 {
    input
        .lines()
        .map(|l| l.parse::<u64>().unwrap())
        .tuple_windows()
        .map(|(a, b, c)| a + b + c)
        .tuple_windows()
        .map(|(sum1, sum2)| (sum2 > sum1) as u64)
        .sum()
}

#[cfg(test)]
mod tests {
    use super::*;

    const EXAMPLE_INPUT: &'static str = "199
200
208
210
200
207
240
269
260
263";

    #[test]
    fn p1_example() {
        assert_eq!(part1(EXAMPLE_INPUT), 7);
    }

    #[test]
    fn p1_input() {
        let input = std::fs::read_to_string("input/day1").expect("reading input file");
        assert_eq!(part1(&input), 1557);
    }

    #[test]
    fn p2_example() {
        assert_eq!(part2(EXAMPLE_INPUT), 5);
    }

    #[test]
    fn p2_input() {
        let input = std::fs::read_to_string("input/day1").expect("reading input file");
        assert_eq!(part2(&input), 1608);
    }
}
