fn char_to_prio(b: u8) -> u8 {
    if b <= b'Z' {
        b - b'A' + 27
    } else {
        b - b'a' + 1
    }
}

fn parse(input: &str) -> impl Iterator<Item = (u64, u64)> + '_ {
    input.lines().map(|line| {
        let item_count = line.len() / 2;

        let mut left = 0u64;
        for &b in &line.as_bytes()[..item_count] {
            left |= 1 << char_to_prio(b);
        }

        let mut right = 0u64;
        for &b in &line.as_bytes()[item_count..] {
            right |= 1 << char_to_prio(b);
        }

        (left, right)
    })
}

pub fn part1(input: &str) -> u64 {
    let mut sum = 0;

    for (left, right) in parse(input) {
        let mut both = left & right;
        for i in 0..=52 {
            if (both & 1) != 0 {
                sum += i;
            }
            both = both >> 1;
        }
    }

    sum
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
