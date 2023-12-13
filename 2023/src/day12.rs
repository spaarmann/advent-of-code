use itertools::Itertools;
use std::{
    collections::{HashMap, VecDeque},
    iter::once,
};

use crate::util::SplitAndParse;

#[derive(Copy, Clone, Eq, PartialEq, Hash, Debug)]
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

type MemoStore = HashMap<(VecDeque<Spring>, VecDeque<usize>, usize), u64>;

fn solve(
    memo: &mut MemoStore,
    springs: &mut VecDeque<Spring>,
    groups: &mut VecDeque<usize>,
    curr_group_len: usize,
) -> u64 {
    if let Some(res) = memo.get(&(springs.clone(), groups.clone(), curr_group_len)) {
        return *res;
    }

    let res = solve_algo(memo, springs, groups, curr_group_len);
    memo.insert((springs.clone(), groups.clone(), curr_group_len), res);
    return res;
}

fn solve_algo(
    memo: &mut MemoStore,
    springs: &mut VecDeque<Spring>,
    groups: &mut VecDeque<usize>,
    curr_group_len: usize,
) -> u64 {
    if springs.is_empty() {
        if groups.is_empty() || groups.len() == 1 && groups[0] == curr_group_len {
            return 1;
        }

        return 0;
    }

    match springs[0] {
        Spring::Operational => {
            if curr_group_len == 0 {
                springs.pop_front();
                let res = solve(memo, springs, groups, 0);
                springs.push_front(Spring::Operational);
                return res;
            } else if curr_group_len == groups[0] {
                springs.pop_front();
                let grp = groups.pop_front().unwrap();
                let res = solve(memo, springs, groups, 0);
                groups.push_front(grp);
                springs.push_front(Spring::Operational);
                return res;
            } else {
                0
            }
        }
        Spring::Damaged => {
            if groups.len() == 0 {
                0
            } else if curr_group_len + 1 > groups[0] {
                0
            } else {
                springs.pop_front();
                let res = solve(memo, springs, groups, curr_group_len + 1);
                springs.push_front(Spring::Damaged);
                return res;
            }
        }
        Spring::Unknown => {
            springs.pop_front();
            springs.push_front(Spring::Damaged);
            let solutions_damaged = solve(memo, springs, groups, curr_group_len);
            springs.pop_front();
            springs.push_front(Spring::Operational);
            let solutions_operational = solve(memo, springs, groups, curr_group_len);
            springs.pop_front();
            springs.push_front(Spring::Unknown);
            solutions_damaged + solutions_operational
        }
    }
}

pub fn part1(input: &str) -> u64 {
    parse(input)
        .map(|(springs, groups)| {
            solve(
                &mut MemoStore::new(),
                &mut springs.into(),
                &mut groups.into(),
                0,
            )
        })
        .sum()
}

pub fn part2(input: &str) -> u64 {
    parse(input)
        .map(|(springs, groups)| {
            let slen = springs.len();
            let mut springs = once(Spring::Unknown)
                .chain(springs.into_iter())
                .cycle()
                .skip(1)
                .take(5 * slen + 4)
                .collect();
            let glen = groups.len();
            let mut groups = groups.into_iter().cycle().take(5 * glen).collect();
            solve(&mut MemoStore::new(), &mut springs, &mut groups, 0)
        })
        .sum()
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
        assert_eq!(part1(&input), 7084);
    }

    #[test]
    fn p2_example() {
        assert_eq!(part2(EXAMPLE_INPUT), 525152);
    }

    // This is too slow in debug mode, because the memoization is stupidly inefficient.
    #[cfg(not(debug_assertions))]
    #[test]
    fn p2_input() {
        let input = std::fs::read_to_string("input/day12").expect("reading input file");
        assert_eq!(part2(&input), 8414003326821);
    }
}
