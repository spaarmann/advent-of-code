fn parse(input: &str) -> impl Iterator<Item = usize> + '_ {
    input.lines().map(|l| l.parse().unwrap())
}

pub fn part1(input: &str) -> u64 {
    let mut numbers = parse(input).collect::<Vec<_>>();
    numbers.sort();

    let (mut ones, mut threes) = (0, 0);
    match numbers[0] {
        1 => ones += 1,
        3 => threes += 1,
        _ => panic!("unexpected start!"),
    }

    for i in 1..numbers.len() {
        match numbers[i] - numbers[i - 1] {
            1 => ones += 1,
            3 => threes += 1,
            _ => panic!("unexpected interval!"),
        }
    }

    threes += 1;

    ones * threes as u64
}

pub fn part2(input: &str) -> u64 {
    let mut numbers = parse(input).collect::<Vec<_>>();
    numbers.push(0);
    numbers.sort();

    // Store the number of paths from each jolt value in the input to the output joltage,
    // indexed by the actual jolt value.
    // The upper bound is the largest jolt value + 3, which is the final
    // output joltage.
    let output_joltage = *numbers.last().unwrap() + 3;
    let mut path_counts = vec![0usize; output_joltage + 1];

    // There is exactly one path from the output to itself. (eh)
    *path_counts.last_mut().unwrap() = 1;

    // Sum up possible paths from back to front.
    for &num in numbers.iter().rev() {
        path_counts[num] = path_counts[num + 1] + path_counts[num + 2] + path_counts[num + 3];
    }

    path_counts[0] as u64
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn p1_input() {
        let input = std::fs::read_to_string("input/day10").expect("reading input file");
        assert_eq!(part1(&input), 2380);
    }

    #[test]
    fn p1_example1() {
        assert_eq!(part1(EXAMPLE1_INPUT), 7 * 5);
    }

    #[test]
    fn p2_input() {
        let input = std::fs::read_to_string("input/day10").expect("reading input file");
        assert_eq!(part2(&input), 48358655787008);
    }

    #[test]
    fn p2_example1() {
        assert_eq!(part2(EXAMPLE1_INPUT), 8);
    }

    const EXAMPLE1_INPUT: &'static str = "\
        16\n\
        10\n\
        15\n\
        5\n\
        1\n\
        11\n\
        7\n\
        19\n\
        6\n\
        12\n\
        4";
}
