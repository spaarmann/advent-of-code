use itertools::Itertools;

use crate::util::Grid;

fn find_reflection(xss: Vec<Vec<char>>) -> Option<u64> {
    'xss: for i in 0..xss.len() - 1 {
        if xss[i] == xss[i + 1] {
            let dist_to_edge = usize::min(i, xss.len() - 1 - (i + 1));
            for j in 1..=dist_to_edge {
                if xss[i - j] != xss[i + 1 + j] {
                    continue 'xss;
                }
            }

            return Some((i + 1) as u64);
        }
    }

    None
}

pub fn part1(input: &str) -> u64 {
    let mut sum = 0;

    for pattern in input.split("\n\n") {
        let grid = Grid::<char>::from_char_grid(pattern);

        let rows = grid.rows().map(|r| r.copied().collect_vec()).collect_vec();
        if let Some(n) = find_reflection(rows) {
            sum += 100 * n;
        } else {
            let cols = grid.cols().map(|c| c.copied().collect_vec()).collect_vec();
            if let Some(n) = find_reflection(cols) {
                sum += n;
            }
        }
    }

    sum
}

pub fn part2(input: &str) -> u64 {
    todo!()
}

#[cfg(test)]
mod tests {
    use super::*;

    const EXAMPLE_INPUT: &'static str = "#.##..##.
..#.##.#.
##......#
##......#
..#.##.#.
..##..##.
#.#.##.#.

#...##..#
#....#..#
..##..###
#####.##.
#####.##.
..##..###
#....#..#
";

    #[test]
    fn p1_example() {
        assert_eq!(part1(EXAMPLE_INPUT), 405);
    }

    #[test]
    fn p1_input() {
        let input = std::fs::read_to_string("input/day13").expect("reading input file");
        assert_eq!(part1(&input), 35691);
    }

    #[test]
    fn p2_example() {
        assert_eq!(part2(EXAMPLE_INPUT), 400);
    }

    #[test]
    fn p2_input() {
        let input = std::fs::read_to_string("input/day13").expect("reading input file");
        assert_eq!(part2(&input), todo!());
    }
}
