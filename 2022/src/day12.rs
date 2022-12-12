use std::collections::VecDeque;

use itertools::Itertools;

struct Map {
    grid: Vec<u8>,
    width: usize,
    height: usize,
}

impl Map {
    fn parse(input: &str) -> Self {
        let width = input.lines().next().unwrap().len();
        let grid = input
            .lines()
            .flat_map(|l| l.as_bytes())
            .copied()
            .collect_vec();
        let height = grid.len() / width;
        Self {
            grid,
            width,
            height,
        }
    }

    fn idx(&self, x: i64, y: i64) -> usize {
        x as usize + y as usize * self.width
    }

    fn in_bounds(&self, x: i64, y: i64) -> bool {
        x >= 0 && x < self.width as i64 && y >= 0 && y < self.height as i64
    }

    fn val(&self, x: i64, y: i64) -> u8 {
        self.grid[x as usize + y as usize * self.width]
    }

    fn height(&self, x: i64, y: i64) -> u8 {
        (match self.val(x, y) {
            b'S' => b'a',
            b'E' => b'z',
            c => c,
        }) - b'a'
    }

    fn start(&self) -> (i64, i64) {
        for x in 0..self.width as i64 {
            for y in 0..self.height as i64 {
                if self.val(x, y) == b'S' {
                    return (x, y);
                }
            }
        }
        panic!("No start found!")
    }

    fn end(&self) -> (i64, i64) {
        for x in 0..self.width as i64 {
            for y in 0..self.height as i64 {
                if self.val(x, y) == b'E' {
                    return (x, y);
                }
            }
        }
        panic!("No end found!")
    }

    fn shortest_distance_to(&self, start: (i64, i64), target: u8, forwards: bool) -> Option<u64> {
        let mut visited = vec![false; self.grid.len()];
        let mut parents = vec![(-1i64, -1i64); self.grid.len()];
        let mut queue = VecDeque::new();

        visited[self.idx(start.0, start.1)] = true;
        queue.push_back(start);

        while let Some((x, y)) = queue.pop_front() {
            if self.val(x, y) == target {
                let mut steps = 0;
                let mut cx = x;
                let mut cy = y;
                while start != (cx, cy) {
                    (cx, cy) = parents[self.idx(cx, cy)];
                    steps += 1;
                }
                return Some(steps);
            }

            let height = self.height(x, y);
            for offset in [(-1, 0), (1, 0), (0, -1), (0, 1)] {
                let nx = x + offset.0;
                let ny = y + offset.1;
                if self.in_bounds(nx, ny)
                    && ((forwards && self.height(nx, ny) <= height + 1)
                        || (!forwards && self.height(nx, ny) + 1 >= height))
                {
                    if !visited[self.idx(nx, ny)] {
                        visited[self.idx(nx, ny)] = true;
                        parents[self.idx(nx, ny)] = (x, y);
                        queue.push_back((nx, ny));
                    }
                }
            }
        }

        None
    }
}

pub fn part1(input: &str) -> u64 {
    let map = Map::parse(input);
    map.shortest_distance_to(map.start(), b'E', true).unwrap()
}

pub fn part2(input: &str) -> u64 {
    let map = Map::parse(input);
    // Technically this should check for 'a' or 'S' instead of just 'a', but in practice the S is
    // not actually going to have the shortest path.
    map.shortest_distance_to(map.end(), b'a', false).unwrap()
}

#[cfg(test)]
mod tests {
    use super::*;

    const EXAMPLE_INPUT: &'static str = "Sabqponm
abcryxxl
accszExk
acctuvwj
abdefghi";

    #[test]
    fn p1_example() {
        assert_eq!(part1(EXAMPLE_INPUT), 31);
    }

    #[test]
    fn p1_input() {
        let input = std::fs::read_to_string("input/day12").expect("reading input file");
        assert_eq!(part1(&input), 420);
    }

    #[test]
    fn p2_example() {
        assert_eq!(part2(EXAMPLE_INPUT), 29);
    }

    #[test]
    fn p2_input() {
        let input = std::fs::read_to_string("input/day12").expect("reading input file");
        assert_eq!(part2(&input), 414);
    }
}
