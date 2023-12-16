use std::collections::HashMap;

use crate::util::Grid;

#[derive(Copy, Clone, PartialEq, Eq, Hash, Debug)]
enum Tile {
    Round,
    Cube,
    Empty,
}

impl From<char> for Tile {
    fn from(c: char) -> Self {
        match c {
            'O' => Tile::Round,
            '#' => Tile::Cube,
            '.' => Tile::Empty,
            _ => panic!("unkwnown tile"),
        }
    }
}

impl From<&Tile> for char {
    fn from(t: &Tile) -> Self {
        match t {
            Tile::Round => 'O',
            Tile::Cube => '#',
            Tile::Empty => '.',
        }
    }
}

#[derive(Copy, Clone, PartialEq, Eq, Debug)]
enum Direction {
    North,
    East,
    South,
    West,
}

fn roll(grid: &mut Grid<Tile>, start: (i64, i64), dir: Direction) {
    if grid[start] != Tile::Round {
        return;
    }
    grid[start] = Tile::Empty;

    let (range, mk_pos, delta, edge): (
        Box<dyn Iterator<Item = i64>>,
        Box<dyn Fn(i64) -> (i64, i64)>,
        _,
        _,
    ) = match dir {
        Direction::North => (
            Box::new((0..start.1).rev()),
            Box::new(move |y| (start.0, y)),
            -1,
            0,
        ),
        Direction::South => (
            Box::new(start.1..grid.height as i64),
            Box::new(move |y| (start.0, y)),
            1,
            grid.height as i64 - 1,
        ),
        Direction::East => (
            Box::new(start.0..grid.width as i64),
            Box::new(move |x| (x, start.1)),
            1,
            grid.width as i64 - 1,
        ),
        Direction::West => (
            Box::new((0..start.0).rev()),
            Box::new(move |x| (x, start.1)),
            -1,
            0,
        ),
    };

    for c in range {
        if grid[mk_pos(c)] != Tile::Empty {
            grid[mk_pos(c - delta)] = Tile::Round;
            return;
        }
    }
    grid[mk_pos(edge)] = Tile::Round;
}

fn tilt_north(grid: &mut Grid<Tile>) {
    for y in 0..grid.height {
        for x in 0..grid.width {
            roll(grid, (x as i64, y as i64), Direction::North);
        }
    }
}
fn tilt_east(grid: &mut Grid<Tile>) {
    for x in (0..grid.width).rev() {
        for y in 0..grid.height {
            roll(grid, (x as i64, y as i64), Direction::East);
        }
    }
}
fn tilt_south(grid: &mut Grid<Tile>) {
    for y in (0..grid.height).rev() {
        for x in 0..grid.width {
            roll(grid, (x as i64, y as i64), Direction::South);
        }
    }
}
fn tilt_west(grid: &mut Grid<Tile>) {
    for x in 0..grid.width {
        for y in 0..grid.height {
            roll(grid, (x as i64, y as i64), Direction::West);
        }
    }
}

fn cycle(grid: &mut Grid<Tile>) {
    tilt_north(grid);
    tilt_west(grid);
    tilt_south(grid);
    tilt_east(grid);
}

fn load(grid: &Grid<Tile>) -> u64 {
    let mut load = 0;
    for y in 0..grid.height {
        for x in 0..grid.width {
            if grid[(x as i64, y as i64)] == Tile::Round {
                load += grid.height - y;
            }
        }
    }

    load as u64
}

pub fn part1(input: &str) -> u64 {
    let mut grid = Grid::<Tile>::from_char_grid(input);

    tilt_north(&mut grid);
    load(&grid)
}

pub fn part2(input: &str) -> u64 {
    let mut grid = Grid::<Tile>::from_char_grid(input);

    let mut seen = HashMap::new();
    seen.insert(grid.clone(), 0);

    let mut i = 1;
    let (loop_start, loop_len) = loop {
        cycle(&mut grid);

        if let Some(loop_start) = seen.get(&grid) {
            break (loop_start, i - loop_start);
        }

        seen.insert(grid.clone(), i);
        i += 1;
    };

    let cycles = 1_000_000_000u64;
    let remaining_after_loop = (cycles - loop_start) % loop_len;

    for _ in 0..remaining_after_loop {
        cycle(&mut grid);
    }

    load(&grid)
}

#[cfg(test)]
mod tests {
    use super::*;

    const EXAMPLE_INPUT: &'static str = "O....#....
O.OO#....#
.....##...
OO.#O....O
.O.....O#.
O.#..O.#.#
..O..#O..O
.......O..
#....###..
#OO..#....
";

    #[test]
    fn p1_example() {
        assert_eq!(part1(EXAMPLE_INPUT), 136);
    }

    #[test]
    fn p1_input() {
        let input = std::fs::read_to_string("input/day14").expect("reading input file");
        assert_eq!(part1(&input), 113424);
    }

    #[test]
    fn p2_example() {
        assert_eq!(part2(EXAMPLE_INPUT), 64);
    }

    #[test]
    fn p2_input() {
        let input = std::fs::read_to_string("input/day14").expect("reading input file");
        assert_eq!(part2(&input), 96003);
    }
}
