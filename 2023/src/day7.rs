use std::cmp::Ordering;

use itertools::Itertools;

const LABELS: &'static str = "23456789TJQKA";

fn parse(input: &str) -> impl Iterator<Item = (&str, i64)> + '_ {
    input.lines().map(|l| {
        let (hand, bid) = l.split_once(' ').unwrap();
        let bid = bid.parse().unwrap();
        (hand, bid)
    })
}

fn group_hand(hand: &str, separate: Option<char>) -> (Vec<i32>, i32) {
    let mut counts = hand
        .chars()
        .filter_map(|c| {
            if Some(c) != separate {
                Some((c, 1))
            } else {
                None
            }
        })
        .into_grouping_map()
        .sum()
        .into_values()
        .collect_vec();
    counts.sort_by(|a, b| b.cmp(a));

    let separated = hand.chars().filter(|c| Some(*c) == separate).count() as i32;

    (counts, separated)
}

fn type_hand(hand: &str, with_jokers: bool) -> i64 {
    let (counts, jokers) = group_hand(hand, if with_jokers { Some('J') } else { None });

    let first = *counts.get(0).unwrap_or(&0);
    let second = *counts.get(1).unwrap_or(&0);
    if first + jokers == 5 {
        6 // five of a kind
    } else if first + jokers == 4 {
        5 // four of a kind
    } else if first + jokers >= 3 && second + (jokers - (3 - first)) >= 2 {
        4 // full house
    } else if first + jokers == 3 {
        3 // three of a kind
    } else if first + jokers >= 2 && second + (jokers - (2 - first)) >= 2 {
        2 // two pair
    } else if first + jokers >= 2 {
        1 // one pair
    } else {
        // high card
        0
    }
}

fn cmp_hands((lt, lh): (i64, &str), (rt, rh): (i64, &str), with_jokers: bool) -> Ordering {
    if lt > rt {
        Ordering::Greater
    } else if lt < rt {
        Ordering::Less
    } else {
        let card_rank = |c| {
            if with_jokers && c == 'J' {
                -1
            } else {
                LABELS.find(c).unwrap() as i64
            }
        };
        for i in 0..5 {
            let lf = card_rank(lh.as_bytes()[i] as char);
            let rf = card_rank(rh.as_bytes()[i] as char);
            let o = lf.cmp(&rf);
            if o != Ordering::Equal {
                return o;
            }
        }
        Ordering::Equal
    }
}

fn solve(input: &str, with_jokers: bool) -> u64 {
    let mut hands = parse(input)
        .map(|(hand, bid)| (type_hand(hand, with_jokers), hand, bid))
        .collect_vec();

    // Rank hands
    hands.sort_by(|l, r| cmp_hands((l.0, l.1), (r.0, r.1), with_jokers));

    hands
        .into_iter()
        .enumerate()
        .map(|(i, (_, _, bid))| (i + 1) as i64 * bid)
        .sum::<i64>() as u64
}

pub fn part1(input: &str) -> u64 {
    solve(input, false)
}

pub fn part2(input: &str) -> u64 {
    solve(input, true)
}

#[cfg(test)]
mod tests {
    use super::*;

    const EXAMPLE_INPUT: &'static str = "32T3K 765
T55J5 684
KK677 28
KTJJT 220
QQQJA 483";

    #[test]
    fn p1_example() {
        assert_eq!(part1(EXAMPLE_INPUT), 6440);
    }

    #[test]
    fn p1_input() {
        let input = std::fs::read_to_string("input/day7").expect("reading input file");
        assert_eq!(part1(&input), 253313241);
    }

    #[test]
    fn p2_example() {
        assert_eq!(part2(EXAMPLE_INPUT), 5905);
    }

    #[test]
    fn p2_input() {
        let input = std::fs::read_to_string("input/day7").expect("reading input file");
        assert_eq!(part2(&input), 253362743);
    }
}
