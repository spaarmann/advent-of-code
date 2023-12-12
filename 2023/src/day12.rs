use itertools::Itertools;

use crate::util::SplitAndParse;

#[derive(Copy, Clone, Eq, PartialEq, Debug)]
enum Spring {
    Unknown,
    Operational,
    Damaged,
}

fn parse(input: &str) -> impl Iterator<Item = (Vec<Spring>, Vec<usize>)> + '_ {
    input.lines().map(|l| {
        let (springs, groups) = l.split_once(' ').unwrap();
        let springs = springs
            .chars()
            .map(|c| match c {
                '.' => Spring::Operational,
                '#' => Spring::Damaged,
                '?' => Spring::Unknown,
                _ => panic!("unknown spring type"),
            })
            .collect_vec();
        let groups = groups.split_and_parse(',').unwrap();
        (springs, groups)
    })
}

fn solve(
    springs: &mut [Spring],
    groups: &[usize],
    pos: usize,
    group_pos: usize,
    curr_group_len: usize,
) -> u64 {
    if pos == springs.len() {
        if group_pos == groups.len() - 1 && curr_group_len == groups[group_pos] {
            return 1;
        }
        if group_pos == groups.len() && curr_group_len == 0 {
            return 1;
        }
    }

    match springs[pos] {
        Spring::Operational => {
            if curr_group_len == 0 {
                solve(springs, groups, pos + 1, group_pos, 0)
            } else if curr_group_len == groups[group_pos] {
                solve(springs, groups, pos + 1, group_pos + 1, 0)
            } else {
                0
            }
        }
        Spring::Damaged => {
            if groups[group_pos] > curr_group_len + 1 {
                0
            } else {
                solve(springs, groups, pos + 1, group_pos, curr_group_len + 1)
            }
        }
        Spring::Unknown => todo!(),
    }
}

pub fn part1(input: &str) -> u64 {
    todo!()
}

pub fn part2(input: &str) -> u64 {
    todo!()
}

#[cfg(test)]
mod tests {
    use super::*;

    const EXAMPLE_INPUT: &'static str = "???.### 1,1,3
.??..??...?##. 1,1,3
?#?#?#?#?#?#?#? 1,3,1,6
????.#...#... 4,1,1
????.######..#####. 1,6,5
?###???????? 3,2,1
";

    #[test]
    fn p1_example() {
        assert_eq!(part1(EXAMPLE_INPUT), 21);
    }

    #[test]
    fn p1_input() {
        let input = std::fs::read_to_string("input/day12").expect("reading input file");
        assert_eq!(part1(&input), todo!());
    }

    #[test]
    fn p2_example() {
        assert_eq!(part2(EXAMPLE_INPUT), todo!());
    }

    #[test]
    fn p2_input() {
        let input = std::fs::read_to_string("input/day12").expect("reading input file");
        assert_eq!(part2(&input), todo!());
    }
}
