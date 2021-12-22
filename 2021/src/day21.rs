use std::collections::HashMap;

fn parse(input: &str) -> (u64, u64) {
    let (first, second) = input.trim().split_once('\n').unwrap();
    (
        first.split_once(": ").unwrap().1.parse().unwrap(),
        second.split_once(": ").unwrap().1.parse().unwrap(),
    )
}

pub fn part1(input: &str) -> u64 {
    let (mut first, mut second) = parse(input);
    // 0-based
    first -= 1;
    second -= 1;

    let mut die = (1..=100).cycle();
    let mut take_three = || {
        [
            die.next().unwrap(),
            die.next().unwrap(),
            die.next().unwrap(),
        ]
        .into_iter()
    };

    let (mut first_score, mut second_score) = (0, 0);
    let mut rolls = 0;
    loop {
        rolls += 3;
        first = (first + take_three().sum::<u64>()) % 10;
        first_score += first + 1;
        if first_score >= 1000 {
            return rolls * second_score;
        }

        rolls += 3;
        second = (second + take_three().sum::<u64>()) % 10;
        second_score += second + 1;
        if second_score >= 1000 {
            return rolls * first_score;
        }
    }
}

// (value, # of possible 3d3 rolls that result in 'value')
const QUANTUM_DICE_OUTCOMES: [(u64, u64); 7] =
    [(3, 1), (4, 3), (5, 6), (6, 7), (7, 6), (8, 3), (9, 1)];

type State = (u64, u64);

pub fn part2(input: &str) -> u64 {
    let (mut first, mut second) = parse(input);
    // 0-based
    first -= 1;
    second -= 1;

    let (first_wins, second_wins) = quantum_play((first, 0), (second, 0), &mut HashMap::new());
    first_wins.max(second_wins)
}

fn quantum_play(
    current: State,
    other: State,
    cache: &mut HashMap<(State, State), (u64, u64)>,
) -> (u64, u64) {
    if other.1 >= 21 {
        return (0, 1);
    }

    if let Some(result) = cache.get(&(current, other)) {
        return *result;
    }

    let mut current_wins = 0;
    let mut other_wins = 0;
    for (val, count) in QUANTUM_DICE_OUTCOMES {
        let new_pos = (current.0 + val) % 10;
        let new_score = current.1 + new_pos + 1;
        let (other_wins_rec, current_wins_rec) = quantum_play(other, (new_pos, new_score), cache);
        current_wins += current_wins_rec * count;
        other_wins += other_wins_rec * count;
    }

    cache.insert((current, other), (current_wins, other_wins));
    (current_wins, other_wins)
}

#[cfg(test)]
mod tests {
    use super::*;

    const EXAMPLE_INPUT: &'static str = "Player 1 starting position: 4
Player 2 starting position: 8
";

    #[test]
    fn p1_example() {
        assert_eq!(part1(EXAMPLE_INPUT), 739785);
    }

    #[test]
    fn p1_input() {
        let input = std::fs::read_to_string("input/day21").expect("reading input file");
        assert_eq!(part1(&input), 853776);
    }

    #[test]
    fn p2_example() {
        assert_eq!(part2(EXAMPLE_INPUT), 444356092776315);
    }

    #[test]
    fn p2_input() {
        let input = std::fs::read_to_string("input/day21").expect("reading input file");
        assert_eq!(part2(&input), 301304993766094);
    }
}
