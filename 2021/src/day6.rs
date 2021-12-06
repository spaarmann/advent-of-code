use na::{RowSVector, SMatrix, SVector};

fn simulate_fish(input: &str, days: u64) -> u64 {
    let mut state = [0; 9];

    for initial_value in input
        .split(',')
        .map(|n| n.trim_end().parse::<usize>().unwrap())
    {
        state[initial_value] += 1;
    }

    let initial_vec = SVector::<f64, 9>::from_vec(state.iter().map(|&n| n as f64).collect());

    // First Approach: Just update bucket-based state the appropriate amount of times.
    for _ in 0..days {
        state.rotate_left(1);
        state[6] += state[8];
    }

    // Second Approach: Do some linear algebra!
    let update_matrix = SMatrix::<f64, 9, 9>::from_rows(&[
        RowSVector::from_row_slice(&[0.0, 1.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0]),
        RowSVector::from_row_slice(&[0.0, 0.0, 1.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0]),
        RowSVector::from_row_slice(&[0.0, 0.0, 0.0, 1.0, 0.0, 0.0, 0.0, 0.0, 0.0]),
        RowSVector::from_row_slice(&[0.0, 0.0, 0.0, 0.0, 1.0, 0.0, 0.0, 0.0, 0.0]),
        RowSVector::from_row_slice(&[0.0, 0.0, 0.0, 0.0, 0.0, 1.0, 0.0, 0.0, 0.0]),
        RowSVector::from_row_slice(&[0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 1.0, 0.0, 0.0]),
        RowSVector::from_row_slice(&[1.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 1.0, 0.0]),
        RowSVector::from_row_slice(&[0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 1.0]),
        RowSVector::from_row_slice(&[1.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0]),
    ]);

    let result_vec = update_matrix.pow(days - 1).unwrap() * initial_vec;

    let result1 = state.iter().sum::<u64>();
    let result2 = result_vec.sum() as u64;

    assert_eq!(result1, result2);
    result1
}

pub fn part1(input: &str) -> u64 {
    simulate_fish(input, 80)
}

pub fn part2(input: &str) -> u64 {
    simulate_fish(input, 256)
}

#[cfg(test)]
mod tests {
    use super::*;

    const EXAMPLE_INPUT: &'static str = "3,4,3,1,2";

    #[test]
    fn p1_example() {
        assert_eq!(part1(EXAMPLE_INPUT), 5934);
    }

    #[test]
    fn p1_input() {
        let input = std::fs::read_to_string("input/day6").expect("reading input file");
        assert_eq!(part1(&input), 386755);
    }

    #[test]
    fn p2_example() {
        assert_eq!(part2(EXAMPLE_INPUT), 26984457539);
    }

    #[test]
    fn p2_input() {
        let input = std::fs::read_to_string("input/day6").expect("reading input file");
        assert_eq!(part2(&input), 1732731810807);
    }
}
