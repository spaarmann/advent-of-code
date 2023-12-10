use std::collections::{HashSet, VecDeque};
use std::iter::once;

use itertools::{izip, Itertools};

use crate::util::Grid;

const PIPE_TILES: &'static [char] = &['|', '-', 'L', 'J', '7', 'F'];
const PIPE_CONNECTIONS: &'static [[(i64, i64); 2]] = &[
    [(0, -1), (0, 1)],
    [(-1, 0), (1, 0)],
    [(0, -1), (1, 0)],
    [(0, -1), (-1, 0)],
    [(0, 1), (-1, 0)],
    [(0, 1), (1, 0)],
];
const DIRS: [(i64, i64); 4] = [(0, 1), (1, 0), (-1, 0), (0, -1)];

fn parse(input: &str) -> Grid<char> {
    Grid::from_char_grid(input)
}

fn next_connection(tile: char, incoming_dir: (i64, i64)) -> Option<(i64, i64)> {
    match tile {
        '.' | 'S' => None,
        pipe => {
            let pipe_idx = PIPE_TILES.iter().position(|&t| t == pipe).unwrap();
            let conns = PIPE_CONNECTIONS[pipe_idx];
            let inv_incoming = (-incoming_dir.0, -incoming_dir.1);
            if conns[0] == inv_incoming {
                Some(conns[1])
            } else if conns[1] == inv_incoming {
                Some(conns[0])
            } else {
                None
            }
        }
    }
}

fn walk(
    grid: &Grid<char>,
    start: (i64, i64),
    start_dir: (i64, i64),
) -> impl Iterator<Item = (i64, i64)> + '_ {
    let first_pos = (start.0 + start_dir.0, start.1 + start_dir.1);
    let first = if grid.in_bounds(first_pos) {
        Some((first_pos, start_dir))
    } else {
        None
    };
    std::iter::successors(first, |&(current, incoming_dir)| {
        match next_connection(grid[current], incoming_dir) {
            None => None,
            Some(conn) => {
                let next = (current.0 + conn.0, current.1 + conn.1);
                if grid.in_bounds(next) {
                    Some((next, conn))
                } else {
                    None
                }
            }
        }
    })
    .map(|(node, _dir)| node)
}

fn make_infinite<T>(mut it: impl Iterator<Item = T>) -> impl Iterator<Item = Option<T>> {
    std::iter::from_fn(move || Some(it.next()))
}

fn find_loop(grid: &Grid<char>, start: (i64, i64)) -> ((i64, i64), i64) {
    let walks = DIRS.map(|dir| walk(&grid, start, dir));

    for (steps, positions) in crate::util::zip_arr(walks.map(make_infinite)).enumerate() {
        let mut meets = positions
            .into_iter()
            .enumerate()
            .tuple_combinations()
            .filter(|&((_, p1), (_, p2))| p1.is_some() && p2.is_some() && p1 == p2);
        if let Some(((walk_idx, _), _)) = meets.next() {
            let loop_max_dist = (steps + 1) as i64;
            return (DIRS[walk_idx], loop_max_dist);
        }
    }

    panic!("could not find loop")
}

pub fn part1(input: &str) -> u64 {
    let grid = parse(input);
    let start = grid.find(|&t| t == 'S').unwrap();

    find_loop(&grid, start).1 as u64
}

#[derive(Copy, Clone, Debug)]
enum Move {
    LeftTurn,
    RightTurn,
    Straight,
}

fn classify_move(d1: (i64, i64), d2: (i64, i64)) -> Move {
    if d1.0 == 0 {
        // starting vertical
        if d2.0 == 0 {
            Move::Straight
        } else if (d1.1 == -1 && d2.0 == 1) || (d1.1 == 1 && d2.0 == -1) {
            Move::RightTurn
        } else if (d1.1 == -1 && d2.0 == -1) || (d1.1 == 1 && d2.0 == 1) {
            Move::LeftTurn
        } else {
            panic!("impossible move")
        }
    } else {
        // starting horizontal
        if d2.1 == 0 {
            Move::Straight
        } else if (d1.0 == -1 && d2.1 == -1) || (d1.0 == 1 && d2.1 == 1) {
            Move::RightTurn
        } else if (d1.0 == -1 && d2.1 == 1) || (d1.0 == 1 && d2.1 == -1) {
            Move::LeftTurn
        } else {
            panic!("impossible move")
        }
    }
}

pub fn part2(input: &str) -> u64 {
    let grid = parse(input);
    let start = grid.find(|&t| t == 'S').unwrap();

    let (loop_start_dir, _) = find_loop(&grid, start);

    let loop_walk = once(start)
        .chain(walk(&grid, start, loop_start_dir))
        .collect_vec();
    let loop_diffs = loop_walk
        .iter()
        .copied()
        .map_windows(|[p1, p2]| (p2.0 - p1.0, p2.1 - p1.1))
        .collect_vec();
    let loop_moves = once(Move::Straight)
        .chain(
            loop_diffs
                .iter()
                .copied()
                .map_windows(|[d1, d2]| classify_move(*d1, *d2)),
        )
        .collect_vec();

    /*
     * .S-7.
     * .|.|.
     * .L-J.
     *
     * loop_walk:  [S | L - J | 7 - S]
     * diffs:       [d d r r u u l l]
     * moves simp:   [s l s l s l s]
     * moves real: [s s l s l s l s]
     *
     * loop_walk:  [S | L - J | 7 - S]
     * diffs:      [d d r r u u l l]
     * moves real: [s s l s l s l s]
     */

    let (ls, rs) = loop_moves.iter().fold((0, 0), |(ls, rs), m| match m {
        Move::Straight => (ls, rs),
        Move::LeftTurn => (ls + 1, rs),
        Move::RightTurn => (ls, rs + 1),
    });

    let left_inside = ls > rs;

    let mut inside = HashSet::new();
    for (&pos, diff, mov) in izip!(&loop_walk, loop_diffs, loop_moves) {
        let fill_offsets = match mov {
            Move::Straight => match (diff, left_inside) {
                ((1, 0), true) | ((-1, 0), false) => [(0, -1), (0, -1)],
                ((1, 0), false) | ((-1, 0), true) => [(0, 1), (0, 1)],
                ((0, 1), true) | ((0, -1), false) => [(1, 0), (1, 0)],
                ((0, 1), false) | ((0, -1), true) => [(-1, 0), (-1, 0)],
                _ => panic!("invalid diff {diff:?}"),
            },
            Move::LeftTurn if !left_inside => match diff {
                (1, 0) => [(-1, 0), (0, 1)],
                (-1, 0) => [(1, 0), (0, -1)],
                (0, 1) => [(-1, 0), (0, -1)],
                (0, -1) => [(1, 0), (0, 1)],
                _ => panic!("invalid diff {diff:?}"),
            },
            Move::RightTurn if left_inside => match diff {
                (1, 0) => [(-1, 0), (0, -1)],
                (-1, 0) => [(1, 0), (0, 1)],
                (0, 1) => [(1, 0), (0, -1)],
                (0, -1) => [(-1, 0), (0, 1)],
                _ => panic!("invalid diff {diff:?}"),
            },
            _ => [(0, 0), (0, 0)],
        };

        let mut work: VecDeque<_> = fill_offsets
            .into_iter()
            .map(|(x, y)| (pos.0 + x, pos.1 + y))
            .collect();
        while let Some(p) = work.pop_front() {
            if inside.contains(&p) || loop_walk.contains(&p) {
                continue;
            }
            inside.insert(p);
            for d in DIRS {
                work.push_back((p.0 + d.0, p.1 + d.1));
            }
        }
    }

    inside.len() as u64
}

#[cfg(test)]
mod tests {
    use super::*;

    const EXAMPLE_INPUT: &'static str = "-L|F7
7S-7|
L|7||
-L-J|
L|-JF";

    #[test]
    fn p1_example1() {
        assert_eq!(part1(EXAMPLE_INPUT), 4);
    }

    const EXAMPLE_INPUT2: &'static str = "7-F7-
.FJ|7
SJLL7
|F--J
LJ.LJ";

    #[test]
    fn p1_example2() {
        assert_eq!(part1(EXAMPLE_INPUT2), 8);
    }

    #[test]
    fn p1_input() {
        let input = std::fs::read_to_string("input/day10").expect("reading input file");
        assert_eq!(part1(&input), 7093);
    }

    #[test]
    fn p2_example1() {
        assert_eq!(part2(EXAMPLE_INPUT), 1);
    }

    #[test]
    fn p2_example2() {
        assert_eq!(part2(EXAMPLE_INPUT2), 1);
    }

    const EXAMPLE_INPUT3: &'static str = "...........
.S-------7.
.|F-----7|.
.||.....||.
.||.....||.
.|L-7.F-J|.
.|..|.|..|.
.L--J.L--J.
...........
";

    #[test]
    fn p2_example3() {
        assert_eq!(part2(EXAMPLE_INPUT3), 4);
    }

    #[test]
    fn p2_input() {
        let input = std::fs::read_to_string("input/day10").expect("reading input file");
        assert_eq!(part2(&input), 407);
    }
}
