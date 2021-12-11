use std::collections::HashSet;

use itertools::Itertools;

const MAP_SIZE: usize = 10;

fn neighbor_positions(x: usize, y: usize) -> impl Iterator<Item = (usize, usize)> {
    let offsets = [
        (-1, -1),
        (-1, 0),
        (-1, 1),
        (0, -1),
        (0, 1),
        (1, -1),
        (1, 0),
        (1, 1),
    ];
    offsets.into_iter().filter_map(move |(ox, oy)| {
        let x = x as i64 + ox;
        let y = y as i64 + oy;
        if x >= 0 && x < MAP_SIZE as i64 && y >= 0 && y < MAP_SIZE as i64 {
            Some((x as usize, y as usize))
        } else {
            None
        }
    })
}

fn run_simulation_until<F: FnMut(&[bool]) -> bool>(input: &str, mut stop: F) -> (u64, u64) {
    let mut map: [u8; MAP_SIZE * MAP_SIZE] = input
        .lines()
        .flat_map(|l| l.chars().map(|c| c.to_digit(10).unwrap() as u8))
        .collect_vec()
        .try_into()
        .unwrap();

    let mut flash_count = 0;
    let mut steps = 0;

    loop {
        let mut flashed = [false; MAP_SIZE * MAP_SIZE];

        // 1. Increas each energy level by 1
        for y in 0..MAP_SIZE {
            for x in 0..MAP_SIZE {
                map[y * MAP_SIZE + x] += 1;
            }
        }

        // 2. Flashing
        let mut to_flash = HashSet::new();
        for y in 0..MAP_SIZE {
            for x in 0..MAP_SIZE {
                if map[y * MAP_SIZE + x] > 9 {
                    to_flash.insert((x, y));
                }
            }
        }
        while to_flash.len() > 0 {
            let (x, y) = to_flash.iter().copied().next().unwrap();
            to_flash.remove(&(x, y));

            flashed[y * MAP_SIZE + x] = true;
            flash_count += 1;

            for (nx, ny) in neighbor_positions(x, y) {
                if !flashed[ny * MAP_SIZE + nx] {
                    map[ny * MAP_SIZE + nx] += 1;
                    if map[ny * MAP_SIZE + nx] > 9 {
                        to_flash.insert((nx, ny));
                    }
                }
            }
        }

        // 3. Reset flashed ones to 0
        for y in 0..MAP_SIZE {
            for x in 0..MAP_SIZE {
                if flashed[y * MAP_SIZE + x] {
                    map[y * MAP_SIZE + x] = 0;
                }
            }
        }

        steps += 1;

        if stop(&flashed) {
            break;
        }
    }

    (flash_count, steps)
}

pub fn part1(input: &str) -> u64 {
    let mut steps = 0;
    let (flash_count, _) = run_simulation_until(input, move |_| {
        steps += 1;
        steps >= 100
    });
    flash_count
}

pub fn part2(input: &str) -> u64 {
    let (_, steps) = run_simulation_until(input, |flashed| flashed.iter().all(|b| *b));
    steps
}

#[cfg(test)]
mod tests {
    use super::*;

    const EXAMPLE_INPUT: &'static str = "5483143223
2745854711
5264556173
6141336146
6357385478
4167524645
2176841721
6882881134
4846848554
5283751526";

    #[test]
    fn p1_example() {
        assert_eq!(part1(EXAMPLE_INPUT), 1656);
    }

    #[test]
    fn p1_input() {
        let input = std::fs::read_to_string("input/day11").expect("reading input file");
        assert_eq!(part1(&input), 1647);
    }

    #[test]
    fn p2_example() {
        assert_eq!(part2(EXAMPLE_INPUT), 195);
    }

    #[test]
    fn p2_input() {
        let input = std::fs::read_to_string("input/day11").expect("reading input file");
        assert_eq!(part2(&input), 348);
    }
}
