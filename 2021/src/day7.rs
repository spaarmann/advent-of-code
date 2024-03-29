fn find_min_cost<F: Fn(u64) -> u64>(input: &str, cost_from_distance: F) -> u64 {
    let numbers: Vec<u64> = input
        .split(',')
        .map(|n| n.trim().parse().unwrap())
        .collect();
    let max = *numbers.iter().max().unwrap();

    let mut costs = vec![0; (max + 1) as usize];

    for n in numbers {
        for p in 0..costs.len() {
            costs[p] += cost_from_distance((n as i64 - p as i64).unsigned_abs());
        }
    }

    *costs.iter().min().unwrap()
}

pub fn part1(input: &str) -> u64 {
    find_min_cost(input, |dist| dist)
}

pub fn part2(input: &str) -> u64 {
    find_min_cost(input, |dist| (dist * (dist + 1) / 2))
}

#[cfg(test)]
mod tests {
    use super::*;

    const EXAMPLE_INPUT: &'static str = "16,1,2,0,4,2,7,1,2,14";

    #[test]
    fn p1_example() {
        assert_eq!(part1(EXAMPLE_INPUT), 37);
    }

    #[test]
    fn p1_input() {
        let input = std::fs::read_to_string("input/day7").expect("reading input file");
        assert_eq!(part1(&input), 337833);
    }

    #[test]
    fn p2_example() {
        assert_eq!(part2(EXAMPLE_INPUT), 168);
    }

    #[test]
    fn p2_input() {
        let input = std::fs::read_to_string("input/day7").expect("reading input file");
        assert_eq!(part2(&input), 96678050);
    }
}
