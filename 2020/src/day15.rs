use std::collections::HashMap;

fn parse(input: &str) -> impl Iterator<Item = u64> + '_ {
    input.trim().split(',').map(|s| s.parse().unwrap())
}

fn run(input: &str, end: u64) -> u64 {
    let mut numbers = HashMap::new();

    let mut turn = 0;
    let mut last_turn_before = 0;
    for i in parse(input) {
        turn += 1;
        last_turn_before = numbers.get(&i).copied().unwrap_or(0);
        numbers.insert(i, turn);
    }

    let mut new_number = 0;
    while turn < end {
        turn += 1;

        new_number = if last_turn_before == 0 {
            0
        } else {
            turn - last_turn_before - 1
        };

        last_turn_before = numbers.get(&new_number).copied().unwrap_or(0);
        numbers.insert(new_number, turn);
    }

    new_number
}

pub fn part1(input: &str) -> u64 {
    run(input, 2020)
}

pub fn part2(input: &str) -> u64 {
    run(input, 30000000)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn p1_input() {
        let input = std::fs::read_to_string("input/day15").expect("reading input file");
        assert_eq!(part1(&input), 755);
    }

    #[test]
    fn p1_example1() {
        assert_eq!(
            &EXAMPLE_INPUTS.iter().map(|s| part1(*s)).collect::<Vec<_>>(),
            &[436, 1, 10, 27, 78, 438, 1836]
        );
    }

    // These take a little long, so we don't run them by default :)

    //#[test]
    //fn p2_input() {
    //    let input = std::fs::read_to_string("input/day15").expect("reading input file");
    //    assert_eq!(part2(&input), 11962);
    //}

    //#[test]
    //fn p2_example1() {
    //    assert_eq!(part2(EXAMPLE_INPUTS[0]), 175594);
    //    assert_eq!(
    //        &EXAMPLE_INPUTS.iter().map(|s| part2(*s)).collect::<Vec<_>>(),
    //        &[175594, 2578, 3544142, 261214, 6895259, 18, 362]
    //    );
    //}

    const EXAMPLE_INPUTS: [&'static str; 7] = [
        "0,3,6", "1,3,2", "2,1,3", "1,2,3", "2,3,1", "3,2,1", "3,1,2",
    ];
}
