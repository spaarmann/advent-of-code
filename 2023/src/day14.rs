use crate::util::Grid;

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
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

fn roll_north(grid: &mut Grid<Tile>, start: (i64, i64)) {
    if grid[start] != Tile::Round {
        return;
    }
    grid[start] = Tile::Empty;

    for y in (0..start.1).rev() {
        if grid[(start.0, y)] != Tile::Empty {
            grid[(start.0, y + 1)] = Tile::Round;
            return;
        }
    }
    // Rolled to edge without stopping
    grid[(start.0, 0)] = Tile::Round;
}

pub fn part1(input: &str) -> u64 {
    let mut grid = Grid::<Tile>::from_char_grid(input);

    for y in 0..grid.height {
        for x in 0..grid.width {
            roll_north(&mut grid, (x as i64, y as i64));
        }
    }

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

pub fn part2(input: &str) -> u64 {
    todo!()
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
        assert_eq!(part2(EXAMPLE_INPUT), todo!());
    }

    #[test]
    fn p2_input() {
        let input = std::fs::read_to_string("input/day14").expect("reading input file");
        assert_eq!(part2(&input), todo!());
    }
}
