fn parse(input: &str) -> impl Iterator<Item = (&str, u64)> + '_ {
    input
        .lines()
        .map(|l| l.split_once(' ').unwrap())
        .map(|(dir, number)| (dir, number.parse().unwrap()))
}

pub fn part1(input: &str) -> u64 {
    let mut horizontal = 0;
    let mut depth = 0;

    for (dir, num) in parse(input) {
        match dir {
            "forward" => horizontal += num,
            "down" => depth += num,
            "up" => depth -= num,
            _ => panic!("invalid input!"),
        }
    }

    horizontal * depth
}

pub fn part2(input: &str) -> u64 {
    let mut aim = 0;
    let mut horizontal = 0;
    let mut depth = 0;

    for (dir, num) in parse(input) {
        match dir {
            "forward" => {
                horizontal += num;
                depth += aim * num;
            }
            "down" => aim += num,
            "up" => aim -= num,
            _ => panic!("invalid input!"),
        }
    }

    horizontal * depth
}

#[cfg(test)]
mod tests {
    use super::*;

    const EXAMPLE_INPUT: &'static str = "forward 5
down 5
forward 8
up 3
down 8
forward 2
";

    #[test]
    fn p1_example() {
        assert_eq!(part1(EXAMPLE_INPUT), 150);
    }

    #[test]
    fn p1_input() {
        let input = std::fs::read_to_string("input/day2").expect("reading input file");
        assert_eq!(part1(&input), 2070300);
    }

    #[test]
    fn p2_example() {
        assert_eq!(part2(EXAMPLE_INPUT), 900);
    }

    #[test]
    fn p2_input() {
        let input = std::fs::read_to_string("input/day2").expect("reading input file");
        assert_eq!(part2(&input), 2078985210);
    }
}
