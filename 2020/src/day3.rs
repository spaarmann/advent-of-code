struct Map {
    map: Vec<bool>,
    width: u64,
}

impl Map {
    // Gets the value at a given position, automatically handling the repetition of the map to the
    // right.
    // (0, 0) is the top left corner.
    fn get(&self, x: u64, y: u64) -> Option<&bool> {
        let x = x % self.width;
        self.map.get((x + y * self.width) as usize)
    }
}

fn parse(input: &str) -> Map {
    let map = input
        .chars()
        .filter(|c| !c.is_ascii_control())
        .map(|c| c == '#')
        .collect();
    let width = input.lines().next().unwrap().len() as u64;

    Map { map, width }
}

fn check_slope(map: &Map, slope: (u64, u64)) -> u64 {
    let mut pos = (0, 0);
    let mut trees = 0;
    while let Some(&tree) = map.get(pos.0, pos.1) {
        if tree {
            trees += 1;
        }

        pos.0 += slope.0;
        pos.1 += slope.1;
    }
    trees
}

pub fn part1(input: &str) -> u64 {
    let map = parse(input);
    check_slope(&map, (3, 1))
}

pub fn part2(input: &str) -> u64 {
    let map = parse(input);

    check_slope(&map, (1, 1))
        * check_slope(&map, (3, 1))
        * check_slope(&map, (5, 1))
        * check_slope(&map, (7, 1))
        * check_slope(&map, (1, 2))
}

#[cfg(test)]
mod tests {
    use super::*;

    const EXAMPLE1_INPUT: &'static str = "..##.......\n\
                            #...#...#..\n\
                            .#....#..#.\n\
                            ..#.#...#.#\n\
                            .#...##..#.\n\
                            ..#.##.....\n\
                            .#.#.#....#\n\
                            .#........#\n\
                            #.##...#...\n\
                            #...##....#\n\
                            .#..#...#.#";

    #[test]
    fn p1_input() {
        let input = std::fs::read_to_string("input/day3").expect("reading input file");
        assert_eq!(part1(&input), 218);
    }

    #[test]
    fn p1_example1() {
        assert_eq!(part1(EXAMPLE1_INPUT), 7);
    }

    #[test]
    fn p2_input() {
        let input = std::fs::read_to_string("input/day3").expect("reading input file");
        assert_eq!(part2(&input), 3847183340);
    }

    #[test]
    fn p2_example1() {
        assert_eq!(part2(EXAMPLE1_INPUT), 336);
    }
}
