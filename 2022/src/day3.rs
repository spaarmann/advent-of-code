fn char_to_prio(b: u8) -> u8 {
    if b <= b'Z' {
        b - b'A' + 27
    } else {
        b - b'a' + 1
    }
}

fn bitset(s: &str) -> u64 {
    s.bytes()
        .map(|b| char_to_prio(b))
        .map(|p| 1 << p)
        .fold(0, |a, b| a | b)
}

fn parse(input: &str) -> impl Iterator<Item = (u64, u64)> + '_ {
    input.lines().map(|line| {
        let (left, right) = line.split_at(line.len() / 2);
        (bitset(left), bitset(right))
    })
}

pub fn part1(input: &str) -> u64 {
    parse(input)
        .map(|(l, r)| l & r)
        .map(|both| both.trailing_zeros() as u64)
        .sum()
}

pub fn part2(input: &str) -> u64 {
    parse(input)
        .map(|(l, r)| l | r)
        .array_chunks()
        .map(|[e1, e2, e3]| e1 & e2 & e3)
        .map(|all| all.trailing_zeros() as u64)
        .sum()
}

#[cfg(test)]
mod tests {
    use super::*;

    const EXAMPLE_INPUT: &'static str = "vJrwpWtwJgWrhcsFMMfFFhFp
jqHRNqRjqzjGDLGLrsFMfFZSrLrFZsSL
PmmdzqPrVvPwwTWBwg
wMqvLMZHhHMvwLHjbvcjnnSBnvTQFn
ttgJtRGJQctTZtZT
CrZsJsPPZsGzwwsLwLmpwMDw";

    #[test]
    fn p1_example() {
        assert_eq!(part1(EXAMPLE_INPUT), 157);
    }

    #[test]
    fn p1_input() {
        let input = std::fs::read_to_string("input/day3").expect("reading input file");
        assert_eq!(part1(&input), 7824);
    }

    #[test]
    fn p2_example() {
        assert_eq!(part2(EXAMPLE_INPUT), 70);
    }

    #[test]
    fn p2_input() {
        let input = std::fs::read_to_string("input/day3").expect("reading input file");
        assert_eq!(part2(&input), 2798);
    }
}
