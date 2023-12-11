use itertools::Itertools;

use crate::util::Grid;

fn solve(input: &str, expansion: usize) -> u64 {
    let grid = Grid::<char>::from_char_grid(input);

    let mut galaxies = grid.positions(|&c| c == '#').collect_vec();
    let empty_rows = (0..grid.height).filter(|&y| (0..grid.width).all(|x| grid[(x, y)] == '.'));
    let empty_cols = (0..grid.width).filter(|&x| (0..grid.height).all(|y| grid[(x, y)] == '.'));

    for (empty_idx, empty_y) in empty_rows.enumerate() {
        for galaxy in &mut galaxies {
            if galaxy.1 as usize > (empty_y + empty_idx * (expansion - 1)) {
                galaxy.1 += (expansion - 1) as i64;
            }
        }
    }
    for (empty_idx, empty_x) in empty_cols.enumerate() {
        for galaxy in &mut galaxies {
            if galaxy.0 as usize > (empty_x + empty_idx * (expansion - 1)) {
                galaxy.0 += (expansion - 1) as i64;
            }
        }
    }

    galaxies
        .into_iter()
        .tuple_combinations()
        .map(|(p1, p2)| p1.0.abs_diff(p2.0) + p1.1.abs_diff(p2.1))
        .sum()
}

pub fn part1(input: &str) -> u64 {
    solve(input, 2)
}

pub fn part2(input: &str) -> u64 {
    solve(input, 1000000)
}

#[cfg(test)]
mod tests {
    use super::*;

    const EXAMPLE_INPUT: &'static str = "...#......
.......#..
#.........
..........
......#...
.#........
.........#
..........
.......#..
#...#.....
";

    #[test]
    fn p1_example() {
        assert_eq!(part1(EXAMPLE_INPUT), 374);
    }

    #[test]
    fn p1_input() {
        let input = std::fs::read_to_string("input/day11").expect("reading input file");
        assert_eq!(part1(&input), 10228230);
    }

    #[test]
    fn p2_example1() {
        assert_eq!(solve(EXAMPLE_INPUT, 10), 1030);
    }

    #[test]
    fn p2_example2() {
        assert_eq!(solve(EXAMPLE_INPUT, 100), 8410);
    }

    #[test]
    fn p2_input() {
        let input = std::fs::read_to_string("input/day11").expect("reading input file");
        assert_eq!(part2(&input), 447073334102);
    }
}
