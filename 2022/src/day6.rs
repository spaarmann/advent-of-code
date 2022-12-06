use itertools::Itertools;

fn not_all_different<const N: usize>(arr: &&[u8; N]) -> bool {
    arr.iter().duplicates().count() != 0
}

fn find_start_marker<const N: usize>(input: &str) -> usize {
    input
        .as_bytes()
        .array_windows()
        .take_while(not_all_different::<N>)
        .count()
        + N
}

pub fn part1(input: &str) -> u64 {
    find_start_marker::<4>(input) as u64
}

pub fn part2(input: &str) -> u64 {
    find_start_marker::<14>(input) as u64
}

#[cfg(test)]
mod tests {
    use super::*;

    const EXAMPLE_INPUT: &'static str = "mjqjpqmgbljsphdztnvjfqwrcgsmlb";

    #[test]
    fn p1_example() {
        assert_eq!(part1(EXAMPLE_INPUT), 7);
    }

    #[test]
    fn p1_input() {
        let input = std::fs::read_to_string("input/day6").expect("reading input file");
        assert_eq!(part1(&input), 1848);
    }

    #[test]
    fn p2_example() {
        assert_eq!(part2(EXAMPLE_INPUT), 19);
    }

    #[test]
    fn p2_input() {
        let input = std::fs::read_to_string("input/day6").expect("reading input file");
        assert_eq!(part2(&input), 2308);
    }
}
