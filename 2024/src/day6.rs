use std::collections::HashSet;

use crate::util::{Direction, Grid, Vec2I};

fn guard(map: &Grid<char>, start: Vec2I) -> Option<HashSet<Vec2I>> {
    let mut visited = HashSet::new();
    let mut dir = Direction::N;

    let mut walk = map.walk_positions(start, dir);
    while let Some(p) = walk.next() {
        if visited.contains(&(p, dir)) {
            return None;
        }
        if map[p] == '#' {
            let prev_dir = dir;
            dir = prev_dir.rotate_cw90();
            walk = map.walk_positions(p - prev_dir, dir);
            continue;
        }
        visited.insert((p, dir));
    }

    Some(visited.into_iter().map(|(p, _)| p).collect())
}

pub fn part1(input: &str) -> u64 {
    let map = Grid::<char>::from_char_grid(input);
    let start = map.find(|&c| c == '^').unwrap();

    let visited = guard(&map, start).unwrap();

    let mut m = map.clone();
    for p in &visited {
        m[*p] = 'X';
    }
    println!("{}", m.to_char_grid_chars());

    visited.len() as u64
}

pub fn part2(input: &str) -> u64 {
    let map = Grid::<char>::from_char_grid(input);
    let start = map.find(|&c| c == '^').unwrap();

    let mut modified = map.clone();
    let mut count = 0;
    guard(&map, start)
        .unwrap()
        .into_iter()
        .filter(|&p| {
            modified[p] = '#';
            let loops = guard(&modified, start).is_none();
            modified[p] = '.';

            count += 1;
            dbg!(count);

            loops
        })
        .count() as u64
}

#[cfg(test)]
mod tests {
    use super::*;

    const EXAMPLE_INPUT: &'static str = "....#.....
.........#
..........
..#.......
.......#..
..........
.#..^.....
........#.
#.........
......#...";

    #[test]
    fn p1_example() {
        assert_eq!(part1(EXAMPLE_INPUT), 41);
    }

    #[test]
    fn p1_input() {
        let input = std::fs::read_to_string("input/day6").expect("reading input file");
        assert_eq!(part1(&input), 5177);
    }

    #[test]
    fn p2_example() {
        assert_eq!(part2(EXAMPLE_INPUT), 6);
    }

    #[ignore = "a bit too slow in debug mode"]
    #[test]
    fn p2_input() {
        let input = std::fs::read_to_string("input/day6").expect("reading input file");
        assert_eq!(part2(&input), 1686);
    }
}
