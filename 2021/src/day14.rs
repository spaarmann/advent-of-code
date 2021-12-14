use std::collections::HashMap;

use itertools::{Itertools, MinMaxResult};

fn calculate_polymer(input: &str, steps: u64) -> u64 {
    let (start, patterns) = input.split_once("\n\n").unwrap();
    let patterns = patterns
        .lines()
        .map(|l| {
            let (pair, insert) = l.split_once(" -> ").unwrap();
            let mut pair = pair.chars();
            let pair = (pair.next().unwrap(), pair.next().unwrap());
            (pair, insert.chars().next().unwrap())
        })
        .collect::<HashMap<_, _>>();

    let mut pattern_counts = HashMap::<(char, char), u64>::new();
    for (a, b) in start.chars().tuple_windows() {
        *pattern_counts.entry((a, b)).or_default() += 1;
    }

    let mut char_counts = HashMap::<char, u64>::new();
    for c in start.chars() {
        *char_counts.entry(c).or_default() += 1;
    }

    let mut new_counts = HashMap::new();
    for _ in 0..steps {
        for (pattern, count) in pattern_counts.iter() {
            if let Some(insert) = patterns.get(pattern) {
                *new_counts.entry((pattern.0, *insert)).or_default() += *count;
                *new_counts.entry((*insert, pattern.1)).or_default() += *count;

                *char_counts.entry(*insert).or_default() += *count;
            } else {
                *new_counts.entry(*pattern).or_default() += *count;
            }
        }

        (pattern_counts, new_counts) = (new_counts, pattern_counts);
        new_counts.clear();
    }

    match char_counts.into_iter().map(|(_, c)| c).minmax() {
        MinMaxResult::MinMax(min, max) => max - min,
        _ => panic!("invalid minmax result"),
    }
}

pub fn part1(input: &str) -> u64 {
    calculate_polymer(input, 10)
}

pub fn part2(input: &str) -> u64 {
    calculate_polymer(input, 40)
}

#[cfg(test)]
mod tests {
    use super::*;

    const EXAMPLE_INPUT: &'static str = "NNCB

CH -> B
HH -> N
CB -> H
NH -> C
HB -> C
HC -> B
HN -> C
NN -> C
BH -> H
NC -> B
NB -> B
BN -> B
BB -> N
BC -> B
CC -> N
CN -> C";

    #[test]
    fn p1_example() {
        assert_eq!(part1(EXAMPLE_INPUT), 1588);
    }

    #[test]
    fn p1_input() {
        let input = std::fs::read_to_string("input/day14").expect("reading input file");
        assert_eq!(part1(&input), 2745);
    }

    #[test]
    fn p2_example() {
        assert_eq!(part2(EXAMPLE_INPUT), 2188189693529);
    }

    #[test]
    fn p2_input() {
        let input = std::fs::read_to_string("input/day14").expect("reading input file");
        assert_eq!(part2(&input), 3420801168962);
    }
}
