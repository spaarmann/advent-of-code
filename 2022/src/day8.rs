use std::{collections::HashSet, ops::ControlFlow};

use itertools::Itertools;

struct Forest {
    grid: Vec<i32>,
    width: isize,
    height: isize,
}

impl Forest {
    fn from_str(input: &str) -> Self {
        let width = input.lines().next().unwrap().len() as isize;
        let grid = input
            .lines()
            .map(|l| l.chars().map(|c| c.to_digit(10).unwrap() as i32))
            .flatten()
            .collect_vec();
        let height = grid.len() as isize / width;

        Self {
            grid,
            width,
            height,
        }
    }

    fn in_bounds(&self, pos: (isize, isize)) -> bool {
        pos.0 >= 0 && pos.0 < self.width && pos.1 >= 0 && pos.1 < self.height
    }

    fn height_at(&self, pos: (isize, isize)) -> i32 {
        self.grid[(pos.0 + pos.1 * self.width) as usize]
    }

    fn walk<F: FnMut((isize, isize), i32) -> ControlFlow<()>>(
        &self,
        start: (isize, isize),
        dir: (isize, isize),
        mut visit: F,
    ) {
        let mut current = start;

        while self.in_bounds(current) {
            let val = self.height_at(current);

            if let ControlFlow::Break(()) = visit(current, val) {
                break;
            }

            current.0 += dir.0;
            current.1 += dir.1;
        }
    }
}

pub fn part1(input: &str) -> u64 {
    let forest = Forest::from_str(input);
    let mut hits = HashSet::new();

    fn visitor(
        visible: &mut HashSet<(isize, isize)>,
    ) -> impl FnMut((isize, isize), i32) -> ControlFlow<()> + '_ {
        let mut max_height_so_far = -1;
        move |pos, height| {
            if height > max_height_so_far {
                visible.insert(pos);
                max_height_so_far = height;
            }
            ControlFlow::Continue(())
        }
    }

    for x in 0..forest.width {
        forest.walk((x, 0), (0, 1), visitor(&mut hits));
        forest.walk((x, forest.height - 1), (0, -1), visitor(&mut hits));
    }
    for y in 0..forest.height {
        forest.walk((0, y), (1, 0), visitor(&mut hits));
        forest.walk((forest.width - 1, y), (-1, 0), visitor(&mut hits));
    }

    hits.len() as u64
}

pub fn part2(input: &str) -> u64 {
    let forest = Forest::from_str(input);

    fn visitor(
        start_height: i32,
        view_dist: &mut i32,
    ) -> impl FnMut((isize, isize), i32) -> ControlFlow<()> + '_ {
        move |_pos, height| {
            *view_dist += 1;
            if height < start_height {
                ControlFlow::Continue(())
            } else {
                ControlFlow::Break(())
            }
        }
    }

    let mut max_score = 0;

    for x in 1..(forest.width - 1) {
        for y in 1..(forest.height - 1) {
            let height = forest.height_at((x, y));

            let dirs = [(1, 0), (-1, 0), (0, 1), (0, -1)];

            let score = dirs
                .into_iter()
                .map(|dir| {
                    let mut dist = 0;
                    forest.walk((x + dir.0, y + dir.1), dir, visitor(height, &mut dist));
                    dist
                })
                .product();

            if score > max_score {
                max_score = score;
            }
        }
    }

    max_score as u64
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
        assert_eq!(part2(EXAMPLE_INPUT), 8);
    }

    #[test]
    fn p2_input() {
        let input = std::fs::read_to_string("input/day8").expect("reading input file");
        assert_eq!(part2(&input), 259308);
    }
}
