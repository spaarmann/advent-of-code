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

fn type_hand(hand: &str) -> i64 {
    let mut counts = hand
        .chars()
        .map(|c| (c, 1))
        .into_grouping_map()
        .sum()
        .into_values()
        .collect_vec();
    counts.sort_by(|a, b| b.cmp(a));

    if let [5] = &counts[..] {
        6 // five of a kind
    } else if let [4, _] = &counts[..] {
        5 // four of a kind
    } else if let [3, 2] = &counts[..] {
        4 // full house
    } else if let [3, 1, 1] = &counts[..] {
        3 // three of a kind
    } else if let [2, 2, 1] = &counts[..] {
        2 // two pair
    } else if let [2, 1, 1, 1] = &counts[..] {
        1 // one pair
    } else {
        // high card
        0
    }
}

fn cmp_hands((lt, lh): (i64, &str), (rt, rh): (i64, &str)) -> Ordering {
    if lt > rt {
        Ordering::Greater
    } else if lt < rt {
        Ordering::Less
    } else {
        for i in 0..5 {
            let lf = LABELS.find(lh.as_bytes()[i] as char).unwrap();
            let rf = LABELS.find(rh.as_bytes()[i] as char).unwrap();
            let o = lf.cmp(&rf);
            if o != Ordering::Equal {
                return o;
            }
        }
        Ordering::Equal
    }
}

pub fn part1(input: &str) -> u64 {
    let mut hands = parse(input)
        .map(|(hand, bid)| (type_hand(hand), hand, bid))
        .collect_vec();

    // Rank hands
    hands.sort_by(|l, r| cmp_hands((l.0, l.1), (r.0, r.1)));

    hands
        .into_iter()
        .enumerate()
        .map(|(i, (_, _, bid))| (i + 1) as i64 * bid)
        .sum::<i64>() as u64
}

pub fn part2(input: &str) -> u64 {
    todo!()
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
        assert_eq!(part2(&input), todo!());
    }
}
