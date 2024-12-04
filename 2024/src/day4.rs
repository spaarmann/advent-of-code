use itertools::Itertools;

use crate::util::{Direction, Grid};

pub fn part1(input: &str) -> u64 {
    let grid = Grid::<char>::from_char_grid(input);
    grid.positions(|&c| c == 'X')
        .cartesian_product(Direction::ALL.iter())
        .filter(|&(p, &dir)| grid.walk(p, dir).collect::<String>().starts_with("XMAS"))
        .count() as u64
}

pub fn part2(input: &str) -> u64 {
    use Direction::*;

    let grid = Grid::<char>::from_char_grid(input);
    grid.positions(|&c| c == 'A')
        .cartesian_product([(NW, NE), (NE, SE), (SE, SW), (SW, NW)].iter())
        .filter(|&(p, &(dir1, dir2))| {
            grid.get(p + dir1) == Some(&'M')
                && grid.get(p + dir1.inverse()) == Some(&'S')
                && grid.get(p + dir2) == Some(&'M')
                && grid.get(p + dir2.inverse()) == Some(&'S')
        })
        .count() as u64
}

#[cfg(test)]
mod tests {
    use super::*;

    const EXAMPLE_INPUT: &'static str = "MMMSXXMASM
MSAMXMSMSA
AMXSXMAAMM
MSAMASMSMX
XMASAMXAMM
XXAMMXXAMA
SMSMSASXSS
SAXAMASAAA
MAMMMXMMMM
MXMXAXMASX";

    #[test]
    fn p1_example() {
        assert_eq!(part1(EXAMPLE_INPUT), 18);
    }

    #[test]
    fn p1_input() {
        let input = std::fs::read_to_string("input/day4").expect("reading input file");
        assert_eq!(part1(&input), 2644);
    }

    #[test]
    fn p2_example() {
        assert_eq!(part2(EXAMPLE_INPUT), 9);
    }

    #[test]
    fn p2_input() {
        let input = std::fs::read_to_string("input/day4").expect("reading input file");
        assert_eq!(part2(&input), 1952);
    }
}
