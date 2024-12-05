use itertools::Itertools;

use crate::util::SplitAndParse;

fn parse(
    input: &str,
) -> (
    impl Iterator<Item = (u64, u64)> + '_,
    impl Iterator<Item = Vec<u64>> + '_,
) {
    let (rules, updates) = input.split_once("\n\n").unwrap();

    let rules = rules.lines().map(|l| l.split_once_and_parse('|'));
    let updates = updates
        .lines()
        .map(|l| l.split_and_parse(',').collect_vec());

    (rules, updates)
}

fn is_correct(update: &[u64], rules: &[(u64, u64)]) -> bool {
    rules.iter().all(|(fst, snd)| {
        match (
            update.iter().position(|u| u == fst),
            update.iter().position(|u| u == snd),
        ) {
            (Some(fst_idx), Some(snd_idx)) => fst_idx < snd_idx,
            _ => true,
        }
    })
}

pub fn part1(input: &str) -> u64 {
    let (rules, updates) = parse(input);
    let rules = rules.collect_vec();

    updates
        .filter(|update| is_correct(update, &rules))
        .map(|update| update[update.len() / 2])
        .sum()
}

pub fn part2(input: &str) -> u64 {
    let (rules, updates) = parse(input);
    let rules = rules.collect_vec();

    updates
        .filter(|update| !is_correct(update, &rules))
        .map(|mut remaining| {
            let mut sorted = Vec::with_capacity(remaining.len());
            while remaining.len() > 0 {
                for i in 0..remaining.len() {
                    let n = remaining[i];
                    let can_insert = rules
                        .iter()
                        .all(|&(l, r)| r != n || sorted.contains(&l) || !remaining.contains(&l));
                    if can_insert {
                        sorted.push(n);
                        remaining.remove(i);
                        break;
                    }
                }
            }
            sorted
        })
        .map(|update| update[update.len() / 2])
        .sum()
}

#[cfg(test)]
mod tests {
    use super::*;

    const EXAMPLE_INPUT: &'static str = "47|53
97|13
97|61
97|47
75|29
61|13
75|53
29|13
97|29
53|29
61|53
97|53
61|29
47|13
75|47
97|75
47|61
75|61
47|29
75|13
53|13

75,47,61,53,29
97,61,53,29,13
75,29,13
75,97,47,61,53
61,13,29
97,13,75,29,47
";

    #[test]
    fn p1_example() {
        assert_eq!(part1(EXAMPLE_INPUT), 143);
    }

    #[test]
    fn p1_input() {
        let input = std::fs::read_to_string("input/day5").expect("reading input file");
        assert_eq!(part1(&input), 4185);
    }

    #[test]
    fn p2_example() {
        assert_eq!(part2(EXAMPLE_INPUT), 123);
    }

    #[test]
    fn p2_input() {
        let input = std::fs::read_to_string("input/day5").expect("reading input file");
        assert_eq!(part2(&input), 4480);
    }
}
