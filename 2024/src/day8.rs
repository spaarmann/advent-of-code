use std::collections::{HashMap, HashSet};

use itertools::Itertools;

use crate::util::Grid;

pub fn solve(input: &str, all_antinodes: bool) -> u64 {
    let map = Grid::<char>::from_char_grid(input);

    let mut antennas = HashMap::new();
    for p in map.positions(|&c| c != '.') {
        antennas.entry(map[p]).or_insert_with(Vec::new).push(p);
    }

    let mut antinodes = HashSet::new();
    for f in antennas.keys() {
        let ps = &antennas[f];
        ps.iter()
            .cartesian_product(ps.iter())
            .filter(|(p1, p2)| p1.1 < p2.1 || (p1.1 == p2.1 && p1.0 < p2.0))
            .for_each(|(&p1, &p2)| {
                let d = p2 - p1;

                if all_antinodes {
                    let mut p = p1;
                    while map.in_bounds(p) {
                        antinodes.insert(p);
                        p = p - d;
                    }
                    let mut p = p1;
                    while map.in_bounds(p) {
                        antinodes.insert(p);
                        p = p + d;
                    }
                } else {
                    antinodes.insert(p1 - d);
                    antinodes.insert(p2 + d);
                }
            });
    }

    antinodes.into_iter().filter(|&p| map.in_bounds(p)).count() as u64
}

pub fn part1(input: &str) -> u64 {
    solve(input, false)
}

pub fn part2(input: &str) -> u64 {
    solve(input, true)
}

#[cfg(test)]
mod tests {
    use super::*;

    const EXAMPLE_INPUT: &'static str = "............
........0...
.....0......
.......0....
....0.......
......A.....
............
............
........A...
.........A..
............
............";

    #[test]
    fn p1_example() {
        assert_eq!(part1(EXAMPLE_INPUT), 14);
    }

    #[test]
    fn p1_input() {
        let input = std::fs::read_to_string("input/day8").expect("reading input file");
        assert_eq!(part1(&input), 247);
    }

    #[test]
    fn p2_example() {
        assert_eq!(part2(EXAMPLE_INPUT), 34);
    }

    #[test]
    fn p2_input() {
        let input = std::fs::read_to_string("input/day8").expect("reading input file");
        assert_eq!(part2(&input), 861);
    }
}
