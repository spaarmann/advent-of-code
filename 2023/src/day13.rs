use itertools::Itertools;

use crate::util::Grid;

fn count_neq(xs: &[char], ys: &[char]) -> usize {
    std::iter::zip(xs, ys).filter(|&(&x, &y)| x != y).count()
}

fn find_reflection(xss: Vec<Vec<char>>, smudge: usize) -> Option<u64> {
    'xss: for i in 0..xss.len() - 1 {
        let mut smudge_remaining = smudge as i64;

        smudge_remaining -= count_neq(&xss[i], &xss[i + 1]) as i64;
        if smudge_remaining >= 0 {
            let dist_to_edge = usize::min(i, xss.len() - 1 - (i + 1));
            for j in 1..=dist_to_edge {
                smudge_remaining -= count_neq(&xss[i - j], &xss[i + 1 + j]) as i64;
                if smudge_remaining < 0 {
                    continue 'xss;
                }
            }

            if smudge_remaining == 0 {
                return Some((i + 1) as u64);
            }
        }
    }

    None
}

fn solve(input: &str, smudge: usize) -> u64 {
    let mut sum = 0;

    for pattern in input.split("\n\n") {
        let grid = Grid::<char>::from_char_grid(pattern);

        let rows = grid.rows().map(|r| r.copied().collect_vec()).collect_vec();
        if let Some(n) = find_reflection(rows, smudge) {
            sum += 100 * n;
        } else {
            let cols = grid.cols().map(|c| c.copied().collect_vec()).collect_vec();
            if let Some(n) = find_reflection(cols, smudge) {
                sum += n;
            }
        }
    }

    sum
}

pub fn part1(input: &str) -> u64 {
    solve(input, 0)
}

pub fn part2(input: &str) -> u64 {
    solve(input, 1)
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
        assert_eq!(part2(&input), 39037);
    }
}
