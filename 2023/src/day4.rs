use std::{collections::HashSet, hash::RandomState};

fn parse(
    input: &str,
) -> impl Iterator<
    Item = (
        impl Iterator<Item = i64> + '_,
        impl Iterator<Item = i64> + '_,
    ),
> {
    input.lines().map(|l| {
        let (winners, have) = l.split_once(": ").unwrap().1.split_once(" | ").unwrap();
        (
            winners.split_whitespace().map(|n| n.parse().unwrap()),
            have.split_whitespace().map(|n| n.parse().unwrap()),
        )
    })
}

fn scores(input: &str) -> impl Iterator<Item = i64> + '_ {
    parse(input).map(|(winners, have)| {
        let winners = HashSet::<i64, RandomState>::from_iter(winners);
        have.filter(|n| winners.contains(n)).count() as i64
    })
}

pub fn part1(input: &str) -> u64 {
    scores(input)
        .map(|winning_count| {
            if winning_count == 0 {
                0
            } else {
                1 << (winning_count - 1)
            }
        })
        .sum::<i64>() as u64
}

pub fn part2(input: &str) -> u64 {
    let mut card_counts = vec![1i64; input.lines().count()];
    for (i, score) in scores(input).enumerate() {
        for j in (i + 1)..(i + 1 + score as usize) {
            card_counts[j] += card_counts[i];
        }
    }

    card_counts.into_iter().sum::<i64>() as u64
}

#[cfg(test)]
mod tests {
    use super::*;

    const EXAMPLE_INPUT: &'static str = "Card 1: 41 48 83 86 17 | 83 86  6 31 17  9 48 53
Card 2: 13 32 20 16 61 | 61 30 68 82 17 32 24 19
Card 3:  1 21 53 59 44 | 69 82 63 72 16 21 14  1
Card 4: 41 92 73 84 69 | 59 84 76 51 58  5 54 83
Card 5: 87 83 26 28 32 | 88 30 70 12 93 22 82 36
Card 6: 31 18 13 56 72 | 74 77 10 23 35 67 36 11
";

    #[test]
    fn p1_example() {
        assert_eq!(part1(EXAMPLE_INPUT), 13);
    }

    #[test]
    fn p1_input() {
        let input = std::fs::read_to_string("input/day4").expect("reading input file");
        assert_eq!(part1(&input), 24542);
    }

    #[test]
    fn p2_example() {
        assert_eq!(part2(EXAMPLE_INPUT), 30);
    }

    #[test]
    fn p2_input() {
        let input = std::fs::read_to_string("input/day4").expect("reading input file");
        assert_eq!(part2(&input), 8736438);
    }
}
