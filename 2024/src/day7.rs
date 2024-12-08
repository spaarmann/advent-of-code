use crate::util::SplitAndParse;

fn parse(input: &str) -> impl Iterator<Item = (u64, Vec<u64>)> + '_ {
    input.lines().map(|l| {
        let (tgt, nums) = l.split_once(": ").unwrap();
        (tgt.parse().unwrap(), nums.split_and_parse(" ").collect())
    })
}

fn solve(tgt: u64, current: u64, nums: &[u64], with_concat: bool) -> bool {
    if current > tgt {
        return false;
    }
    if nums.len() == 0 {
        return tgt == current;
    }

    solve(tgt, current + nums[0], &nums[1..], with_concat)
        || solve(tgt, current * nums[0], &nums[1..], with_concat)
        || (with_concat
            && solve(
                tgt,
                current * 10u64.pow(1 + nums[0].ilog10()) + nums[0],
                &nums[1..],
                with_concat,
            ))
}

pub fn part1(input: &str) -> u64 {
    parse(input)
        .filter(|(tgt, nums)| solve(*tgt, 0, nums, false))
        .map(|(tgt, _)| tgt)
        .sum()
}

pub fn part2(input: &str) -> u64 {
    parse(input)
        .filter(|(tgt, nums)| solve(*tgt, 0, nums, true))
        .map(|(tgt, _)| tgt)
        .sum()
}

#[cfg(test)]
mod tests {
    use super::*;

    const EXAMPLE_INPUT: &'static str = "190: 10 19
3267: 81 40 27
83: 17 5
156: 15 6
7290: 6 8 6 15
161011: 16 10 13
192: 17 8 14
21037: 9 7 18 13
292: 11 6 16 20
";

    #[test]
    fn p1_example() {
        assert_eq!(part1(EXAMPLE_INPUT), 3749);
    }

    #[test]
    fn p1_input() {
        let input = std::fs::read_to_string("input/day7").expect("reading input file");
        assert_eq!(part1(&input), 6392012777720);
    }

    #[test]
    fn p2_example() {
        assert_eq!(part2(EXAMPLE_INPUT), 11387);
    }

    #[test]
    fn p2_input() {
        let input = std::fs::read_to_string("input/day7").expect("reading input file");
        assert_eq!(part2(&input), 61561126043536);
    }
}
