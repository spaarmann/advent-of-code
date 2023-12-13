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
        return 0;
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
            if group_pos == groups.len() {
                0
            } else if curr_group_len + 1 > groups[group_pos] {
                0
            } else {
                solve(springs, groups, pos + 1, group_pos, curr_group_len + 1)
            }
        }
        Spring::Unknown => {
            springs[pos] = Spring::Damaged;
            let solutions_damaged = solve(springs, groups, pos, group_pos, curr_group_len);
            springs[pos] = Spring::Operational;
            let solutions_operational = solve(springs, groups, pos, group_pos, curr_group_len);
            springs[pos] = Spring::Unknown;
            solutions_damaged + solutions_operational
        }
    }
}

pub fn part1(input: &str) -> u64 {
    parse(input)
        .map(|(mut springs, groups)| solve(&mut springs, &groups, 0, 0, 0))
        .sum()
}

pub fn part2(input: &str) -> u64 {
    parse(input)
        .map(|(mut springs, mut groups)| {
            let slen = springs.len();
            springs = springs.into_iter().cycle().take(5 * slen).collect_vec();
            let glen = groups.len();
            groups = groups.into_iter().cycle().take(5 * glen).collect_vec();
            solve(&mut springs, &groups, 0, 0, 0)
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

    #[test]
    fn p2_input() {
        let input = std::fs::read_to_string("input/day12").expect("reading input file");
        assert_eq!(part2(&input), todo!());
    }
}
