pub fn part1(input: &str) -> u64 {
    todo!()
}

pub fn part2(input: &str) -> u64 {
    todo!()
}

#[cfg(test)]
mod tests {
    use super::*;

    const EXAMPLE_INPUT: &'static str = todo!();

    #[test]
    fn p1_example() {
        assert_eq!(part1(EXAMPLE_INPUT), todo!());
    }

    #[test]
    fn p1_input() {
        let input = std::fs::read_to_string("input/day$DAY$").expect("reading input file");
        assert_eq!(part1(&input), todo!());
    }

    #[test]
    fn p2_example() {
        assert_eq!(part2(&input), todo!());
    }

    #[test]
    fn p2_input() {
        let input = std::fs::read_to_string("input/day$DAY$").expect("reading input file");
        assert_eq!(part2(&input), todo!());
    }
}
