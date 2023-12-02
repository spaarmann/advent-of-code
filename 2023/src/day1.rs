pub fn part1(input: &str) -> u64 {
    input
        .lines()
        .map(|l| {
            let digits = l.chars().filter(|c| c.is_digit(10));
            let first = digits.clone().next().unwrap();
            let last = digits.last().unwrap();
            10 * first.to_digit(10).unwrap() + last.to_digit(10).unwrap()
        })
        .sum::<u32>() as u64
}

pub fn part2(input: &str) -> u64 {
    part1(
        &input
            .replace("zero", "z0o")
            .replace("one", "o1e")
            .replace("two", "t2o")
            .replace("three", "t3e")
            .replace("four", "f4r")
            .replace("five", "f5e")
            .replace("six", "s6x")
            .replace("seven", "s7n")
            .replace("eight", "e8t")
            .replace("nine", "n9e"),
    )
}

#[cfg(test)]
mod tests {
    use super::*;

    const EXAMPLE_INPUT: &'static str = "1abc2
pqr3stu8vwx
a1b2c3d4e5f
treb7uchet
";

    const EXAMPLE_INPUT2: &'static str = " two1nine
eightwothree
abcone2threexyz
xtwone3four
4nineeightseven2
zoneight234
7pqrstsixteen
";

    #[test]
    fn p1_example() {
        assert_eq!(part1(EXAMPLE_INPUT), 142);
    }

    #[test]
    fn p1_input() {
        let input = std::fs::read_to_string("input/day1").expect("reading input file");
        assert_eq!(part1(&input), 56397);
    }

    #[test]
    fn p2_example() {
        assert_eq!(part2(EXAMPLE_INPUT2), 281);
    }

    #[test]
    fn p2_input() {
        let input = std::fs::read_to_string("input/day1").expect("reading input file");
        assert_eq!(part2(&input), 55701);
    }
}
