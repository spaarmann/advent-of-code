extern crate nalgebra as na;

use itertools::Itertools;
use na::Vector2;

type Vec2 = Vector2<i64>;

#[derive(Copy, Clone, PartialEq, Eq)]
enum Spot {
    Air,
    Rock,
    Sand,
}

struct Cave {
    min: Vec2,
    max: Vec2,
    size: Vec2,
    map: Vec<Spot>,
}

impl Cave {
    fn from_str(input: &str) -> Self {
        fn paths(s: &str) -> impl Iterator<Item = impl Iterator<Item = Vec2> + '_> + '_ {
            s.lines().map(|l| {
                l.split(" -> ").map(|p| {
                    let (left, right) = p.split_once(',').unwrap();
                    Vec2::new(left.parse().unwrap(), right.parse().unwrap())
                })
            })
        }

        let (min, max) = paths(input).flatten().fold(
            (Vec2::new(i64::MAX, i64::MAX), Vec2::new(i64::MIN, i64::MIN)),
            |(min, max), x| {
                (
                    Vec2::new(min.x.min(x.x), min.y.min(x.y)),
                    Vec2::new(max.x.max(x.x), max.y.max(x.y)),
                )
            },
        );
        #[allow(unused_variables)]
        let min = Vec2::new(min.x, min.y.min(0));
        let max = max + Vec2::new(1, 3);

        // TODO: This is me being too annoyed with part 2 to implement resizing or something properly
        let min = Vec2::new(0, 0);
        let max = max + Vec2::new(100, 0);

        let size = max - min;
        let map = vec![Spot::Air; (size.x * size.y) as usize];
        let mut cave = Self {
            min,
            max,
            size,
            map,
        };

        for path in paths(input) {
            for (start, end) in path.tuple_windows() {
                let delta = (end - start).map(|e| e.signum());
                let mut p = start;
                while p != end {
                    cave.set(p, Spot::Rock);
                    p += delta;
                }
                cave.set(end, Spot::Rock);
            }
        }

        cave
    }

    fn idx(&self, p: Vec2) -> usize {
        let idx = (p.x - self.min.x + (p.y - self.min.y) * self.size.x) as usize;
        //if idx > self.map.len() {
        //    panic!(
        //        "Pos {:?} with idx {} out of bounds with size {:?}, min {:?}, max {:?}",
        //        p, idx, self.size, self.min, self.max
        //    );
        //}
        idx
    }

    fn set(&mut self, p: Vec2, s: Spot) {
        let idx = self.idx(p);
        self.map[idx] = s;
    }

    fn empty(&self, p: Vec2) -> bool {
        self.map[self.idx(p)] == Spot::Air
    }

    fn drop_sand(&mut self, origin: Vec2, with_floor: bool) -> bool {
        const DOWN: Vec2 = Vec2::new(0, 1);
        const DOWN_LEFT: Vec2 = Vec2::new(-1, 1);
        const DOWN_RIGHT: Vec2 = Vec2::new(1, 1);

        if !self.empty(origin) {
            return false;
        }

        let mut p = origin;
        loop {
            if !with_floor && p.y == self.max.y - 3 {
                break false;
            } else if with_floor && p.y == self.max.y - 2 {
                self.set(p, Spot::Sand);
                break true;
            } else if self.empty(p + DOWN) {
                p = p + DOWN;
            } else if self.empty(p + DOWN_LEFT) {
                p = p + DOWN_LEFT;
            } else if self.empty(p + DOWN_RIGHT) {
                p = p + DOWN_RIGHT;
            } else {
                self.set(p, Spot::Sand);
                break true;
            }
        }
    }

    #[allow(dead_code)]
    fn draw(&self) -> String {
        let mut s = String::new();
        let min_x = (self.min.y..self.max.y)
            .map(|y| {
                *&self.map[self.idx(Vec2::new(0, y))..=self.idx(Vec2::new(self.max.x - 1, y))]
                    .iter()
                    .position(|p| p != &Spot::Air)
                    .unwrap_or(0) as i64
            })
            .min()
            .unwrap();
        let max_x = (self.min.y..self.max.y)
            .map(|y| {
                self.max.x
                    - *&self.map
                        [self.idx(Vec2::new(min_x, y))..=self.idx(Vec2::new(self.max.x - 1, y))]
                        .iter()
                        .rev()
                        .position(|p| p == &Spot::Air)
                        .unwrap_or(self.max.x as usize) as i64
            })
            .max()
            .unwrap();
        for y in self.min.y..self.max.y {
            for x in min_x..max_x {
                s.push(match self.map[self.idx(Vec2::new(x, y))] {
                    Spot::Air => ' ',
                    Spot::Rock => '#',
                    Spot::Sand => 'o',
                });
            }
            s.push('\n');
        }
        s
    }
}

pub fn part1(input: &str) -> u64 {
    const SAND_SOURCE: Vec2 = Vec2::new(500, 0);

    let mut cave = Cave::from_str(input);
    std::iter::repeat(())
        .take_while(|()| cave.drop_sand(SAND_SOURCE, false))
        .count() as u64
}

pub fn part2(input: &str) -> u64 {
    const SAND_SOURCE: Vec2 = Vec2::new(500, 0);

    let mut cave = Cave::from_str(input);
    std::iter::repeat(())
        .take_while(|()| cave.drop_sand(SAND_SOURCE, true))
        .count() as u64
}

#[cfg(test)]
mod tests {
    use super::*;

    const EXAMPLE_INPUT: &'static str = "498,4 -> 498,6 -> 496,6
503,4 -> 502,4 -> 502,9 -> 494,9";

    #[test]
    fn p1_example() {
        assert_eq!(part1(EXAMPLE_INPUT), 24);
    }

    #[test]
    fn p1_input() {
        let input = std::fs::read_to_string("input/day14").expect("reading input file");
        assert_eq!(part1(&input), 728);
    }

    #[test]
    fn p2_example() {
        assert_eq!(part2(EXAMPLE_INPUT), 93);
    }

    #[test]
    #[ignore = "too slow"]
    fn p2_input() {
        let input = std::fs::read_to_string("input/day14").expect("reading input file");
        assert_eq!(part2(&input), 27623);
    }
}
