use std::collections::HashSet;

use itertools::Itertools;

fn find_visible(
    grid: &[i32],
    visible: &mut HashSet<(isize, isize)>,
    width: isize,
    height: isize,
    start: (isize, isize),
    dir: (isize, isize),
) {
    let mut current = start;
    let mut max_height_so_far = -1;

    while current.0 >= 0 && current.0 < width && current.1 >= 0 && current.1 < height {
        let val = grid[(current.0 + current.1 * width) as usize];
        if val > max_height_so_far {
            visible.insert(current);
            max_height_so_far = val;
        }

        current.0 += dir.0;
        current.1 += dir.1;
    }
}

pub fn part1(input: &str) -> u64 {
    let width = input.lines().next().unwrap().len() as isize;
    let grid = input
        .lines()
        .map(|l| l.chars().map(|c| c.to_digit(10).unwrap() as i32))
        .flatten()
        .collect_vec();
    let height = grid.len() as isize / width;

    let mut hits = HashSet::new();

    for x in 0..width {
        find_visible(&grid, &mut hits, width, height, (x, 0), (0, 1));
        find_visible(&grid, &mut hits, width, height, (x, height - 1), (0, -1));
    }
    for y in 0..height {
        find_visible(&grid, &mut hits, width, height, (0, y), (1, 0));
        find_visible(&grid, &mut hits, width, height, (width - 1, y), (-1, 0));
    }

    hits.len() as u64
}

pub fn part2(input: &str) -> u64 {
    todo!()
}

#[cfg(test)]
mod tests {
    use super::*;

    const EXAMPLE_INPUT: &'static str = "30373
25512
65332
33549
35390";

    #[test]
    fn p1_example() {
        assert_eq!(part1(EXAMPLE_INPUT), 21);
    }

    #[test]
    fn p1_input() {
        let input = std::fs::read_to_string("input/day8").expect("reading input file");
        assert_eq!(part1(&input), 1798);
    }

    #[test]
    fn p2_example() {
        assert_eq!(part2(EXAMPLE_INPUT), todo!());
    }

    #[test]
    fn p2_input() {
        let input = std::fs::read_to_string("input/day8").expect("reading input file");
        assert_eq!(part2(&input), todo!());
    }
}
