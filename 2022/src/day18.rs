extern crate nalgebra as na;

use crate::util::SplitAndParse;
use miette::{miette, IntoDiagnostic, Result};

type Vec3 = na::Vector3<i64>;

struct Voxels {
    grid: Vec<bool>,
    size: Vec3,
}

impl Voxels {
    fn from_str(s: &str) -> Result<Self> {
        let coords = s.lines().map(|l| l.split_and_parse(','));

        let mut max = Vec3::new(0, 0, 0);

        for coord in coords.clone() {
            let Ok(&[x, y, z]) = coord.as_deref() else {
                return Err(miette!("could not parse"));
            };
            max = max.sup(&Vec3::new(x, y, z))
        }

        let size = max + Vec3::new(1, 1, 1);
        let grid = vec![false; (size.x * size.y * size.z) as usize];

        let mut voxels = Self { grid, size };

        for coord in coords {
            let Ok(&[x, y, z]) = coord.as_deref() else {
                return Err(miette!("could not parse"));
            };
            voxels.set(Vec3::new(x, y, z))
        }

        Ok(voxels)
    }

    fn idx(&self, p: Vec3) -> usize {
        (p.x + self.size.x * (p.z + self.size.z * p.y)) as usize
    }

    fn try_idx(&self, p: Vec3) -> Option<usize> {
        if p.x < 0
            || p.y < 0
            || p.z < 0
            || p.x >= self.size.x
            || p.y >= self.size.y
            || p.z >= self.size.z
        {
            None
        } else {
            Some(self.idx(p))
        }
    }

    fn set(&mut self, p: Vec3) {
        let idx = self.idx(p);
        self.grid[idx] = true;
    }

    fn get(&self, p: Vec3) -> bool {
        self.grid[self.idx(p)]
    }

    fn try_get(&self, p: Vec3) -> Option<bool> {
        self.try_idx(p).map(|idx| self.grid[idx])
    }
}

pub fn part1(input: &str) -> u64 {
    let voxels = Voxels::from_str(input).unwrap();

    let mut count = 0;

    for y in 0..voxels.size.y {
        for z in 0..voxels.size.z {
            for x in 0..voxels.size.x {
                let p = Vec3::new(x, y, z);

                if voxels.get(p) {
                    const SIDES: [Vec3; 6] = [
                        Vec3::new(-1, 0, 0),
                        Vec3::new(1, 0, 0),
                        Vec3::new(0, 0, -1),
                        Vec3::new(0, 0, 1),
                        Vec3::new(0, -1, 0),
                        Vec3::new(0, 1, 0),
                    ];
                    for offset in SIDES {
                        if !voxels.try_get(p + offset).unwrap_or(false) {
                            count += 1;
                        }
                    }
                }
            }
        }
    }

    count
}

pub fn part2(input: &str) -> u64 {
    todo!()
}

#[cfg(test)]
mod tests {
    use super::*;

    const EXAMPLE_INPUT: &'static str = "2,2,2
1,2,2
3,2,2
2,1,2
2,3,2
2,2,1
2,2,3
2,2,4
2,2,6
1,2,5
3,2,5
2,1,5
2,3,5";

    #[test]
    fn p1_example() {
        assert_eq!(part1(EXAMPLE_INPUT), 64);
    }

    #[test]
    fn p1_input() {
        let input = std::fs::read_to_string("input/day18").expect("reading input file");
        assert_eq!(part1(&input), 4322);
    }

    #[test]
    fn p2_example() {
        assert_eq!(part2(EXAMPLE_INPUT), 58);
    }

    #[test]
    fn p2_input() {
        let input = std::fs::read_to_string("input/day18").expect("reading input file");
        assert_eq!(part2(&input), todo!());
    }
}
