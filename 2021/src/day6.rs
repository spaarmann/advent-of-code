use std::ops::DivAssign;

use na::{
    allocator::Allocator, ClosedAdd, ClosedMul, DefaultAllocator, Dim, DimName, Matrix, RowSVector,
    SMatrix, SVector, Scalar,
};

// `pow` and `pow_mut` in nalgebra are kind of broken, and unnecessarily restricted to float
// matrices.
fn pow_mut<T, D, I>(
    m: &mut Matrix<T, D, D, <DefaultAllocator as Allocator<T, D, D>>::Buffer>,
    mut e: I,
) -> bool
where
    T: Scalar + ClosedAdd + ClosedMul + num::Zero + num::One,
    D: Dim + DimName,
    DefaultAllocator: Allocator<T, D, D>,
    I: num::PrimInt + DivAssign,
{
    let zero = I::zero();

    if e == zero {
        // TODO
        return false;
    }

    if e < zero {
        // TODO
        return false;
    }

    let one = I::one();
    let two = I::from(2u8).unwrap();

    let mut buf = m.clone_owned();
    let mut m2 = Matrix::<T, D, D, <DefaultAllocator as Allocator<T, D, D>>::Buffer>::identity();

    while e > one {
        if e % two == zero {
            m.mul_to(&m, &mut buf);
            m.copy_from(&buf);

            e /= two;
        } else {
            m.mul_to(&m2, &mut buf);
            m2.copy_from(&buf);

            m.mul_to(&m, &mut buf);
            m.copy_from(&buf);

            e = (e - one) / two;
        }
    }

    m.mul_to(&m2, &mut buf);
    m.copy_from(&buf);
    true
}

pub fn simulate_fish_iterative(input: &str, days: u64) -> u64 {
    let mut state = [0; 9];

    for initial_value in input
        .split(',')
        .map(|n| n.trim_end().parse::<usize>().unwrap())
    {
        state[initial_value] += 1;
    }

    for _ in 0..days {
        state.rotate_left(1);
        state[6] += state[8];
    }

    state.iter().sum::<u64>()
}

pub fn simulate_fish_linalg(input: &str, days: u64) -> u64 {
    let mut state = [0; 9];

    for initial_value in input
        .split(',')
        .map(|n| n.trim_end().parse::<usize>().unwrap())
    {
        state[initial_value] += 1;
    }

    let initial_vec = SVector::<u64, 9>::from_row_slice(&state);

    let mut update_matrix = SMatrix::<u64, 9, 9>::from_rows(&[
        RowSVector::from_row_slice(&[0, 1, 0, 0, 0, 0, 0, 0, 0]),
        RowSVector::from_row_slice(&[0, 0, 1, 0, 0, 0, 0, 0, 0]),
        RowSVector::from_row_slice(&[0, 0, 0, 1, 0, 0, 0, 0, 0]),
        RowSVector::from_row_slice(&[0, 0, 0, 0, 1, 0, 0, 0, 0]),
        RowSVector::from_row_slice(&[0, 0, 0, 0, 0, 1, 0, 0, 0]),
        RowSVector::from_row_slice(&[0, 0, 0, 0, 0, 0, 1, 0, 0]),
        RowSVector::from_row_slice(&[1, 0, 0, 0, 0, 0, 0, 1, 0]),
        RowSVector::from_row_slice(&[0, 0, 0, 0, 0, 0, 0, 0, 1]),
        RowSVector::from_row_slice(&[1, 0, 0, 0, 0, 0, 0, 0, 0]),
    ]);

    pow_mut(&mut update_matrix, days);
    let result_vec = update_matrix * initial_vec;
    result_vec.sum()
}

pub fn part1(input: &str) -> u64 {
    let iterative = simulate_fish_iterative(input, 80);
    let linalg = simulate_fish_linalg(input, 80);
    assert_eq!(iterative, linalg);
    linalg
}

pub fn part2(input: &str) -> u64 {
    let iterative = simulate_fish_iterative(input, 256);
    let linalg = simulate_fish_linalg(input, 256);
    assert_eq!(iterative, linalg);
    linalg
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
