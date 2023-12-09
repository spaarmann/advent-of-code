use itertools::Itertools;

fn parse(input: &str) -> impl Iterator<Item = impl DoubleEndedIterator<Item = i64> + '_> {
    input
        .lines()
        .map(|l| l.split_whitespace().map(|n| n.parse().unwrap()))
}

fn solve(scan_data: impl Iterator<Item = impl Iterator<Item = i64>>) -> u64 {
    scan_data
        .map(|history| {
            let mut nums = vec![history.collect_vec()];
            while let Some(last) = nums.last()
                && !last.iter().all(|&n| n == last[0])
            {
                nums.push(last.array_windows().map(|[a, b]| b - a).collect_vec());
            }

            for i in (0..nums.len()).rev() {
                let to_add = if i == nums.len() - 1 {
                    0
                } else {
                    *nums[i + 1].last().unwrap()
                };
                let last = *nums[i].last().unwrap();
                nums[i].push(last + to_add);
            }

            *nums[0].last().unwrap()
        })
        .sum::<i64>() as u64
}

pub fn part1(input: &str) -> u64 {
    solve(parse(input))
}

pub fn part2(input: &str) -> u64 {
    solve(parse(input).map(|history| history.rev()))
}

#[cfg(test)]
mod tests {
    use super::*;

    const EXAMPLE_INPUT: &'static str = "0 3 6 9 12 15
1 3 6 10 15 21
10 13 16 21 30 45
";

    #[test]
    fn p1_example() {
        assert_eq!(part1(EXAMPLE_INPUT), 114);
    }

    #[test]
    fn p1_input() {
        let input = std::fs::read_to_string("input/day9").expect("reading input file");
        assert_eq!(part1(&input), 1806615041);
    }

    #[test]
    fn p2_example() {
        assert_eq!(part2(EXAMPLE_INPUT), 2);
    }

    #[test]
    fn p2_input() {
        let input = std::fs::read_to_string("input/day9").expect("reading input file");
        assert_eq!(part2(&input), 1211);
    }
}
