use std::collections::HashSet;

struct Map {
    width: usize,
    height: usize,
    map: Vec<u64>,
}

impl Map {
    fn get(&self, x: i64, y: i64) -> Option<u64> {
        if x < 0 || x >= self.width as i64 || y < 0 || y >= self.height as i64 {
            None
        } else {
            Some(self.map[(y * self.width as i64 + x) as usize])
        }
    }

    fn neighbors(&self, x: i64, y: i64) -> impl Iterator<Item = (i64, i64, u64)> + '_ {
        [(-1, 0), (0, -1), (1, 0), (0, 1)]
            .into_iter()
            .filter_map(move |(x_offset, y_offset)| {
                let x = x + x_offset;
                let y = y + y_offset;
                self.get(x, y).map(|val| (x, y, val))
            })
    }

    fn low_points(&self) -> Vec<(i64, i64, u64)> {
        let mut low_points = Vec::new();

        for y in 0..(self.height as i64) {
            for x in 0..(self.width as i64) {
                let val = self.get(x, y).unwrap();
                if self.neighbors(x, y).all(|(_, _, v)| v > val) {
                    low_points.push((x, y, val));
                }
            }
        }

        low_points
    }
}

fn parse(input: &str) -> Map {
    let width = input.lines().next().unwrap().len();
    let mut map = Vec::new();

    let mut height = 0;
    for line in input.lines() {
        height += 1;

        for c in line.chars() {
            map.push(c.to_digit(10).unwrap() as u64);
        }
    }

    Map { width, height, map }
}

pub fn part1(input: &str) -> u64 {
    let map = parse(input);
    let low_points = map.low_points();

    let risk_sum = low_points.iter().map(|(_, _, val)| val + 1).sum();

    risk_sum
}

pub fn part2(input: &str) -> u64 {
    let map = parse(input);
    let low_points = map.low_points();

    let mut basins = Vec::new();
    for (x, y, _) in low_points {
        let mut queue = HashSet::new();
        let mut basin = HashSet::new();
        queue.insert((x, y));

        while queue.len() > 0 {
            let (x, y) = queue.iter().copied().next().unwrap();
            queue.remove(&(x, y));

            if basin.contains(&(x, y)) {
                continue;
            }

            let val = map.get(x, y).unwrap();
            if val == 9 {
                continue;
            }

            basin.insert((x, y));
            for (xn, yn, _) in map.neighbors(x, y) {
                if !basin.contains(&(xn, yn)) {
                    queue.insert((xn, yn));
                }
            }
        }

        basins.push(basin);
    }

    basins.sort_by_key(|b| b.len());

    basins
        .iter()
        .rev()
        .take(3)
        .map(|b| b.len())
        .product::<usize>() as u64
}

#[cfg(test)]
mod tests {
    use super::*;

    const EXAMPLE_INPUT: &'static str = "2199943210
3987894921
9856789892
8767896789
9899965678
";

    #[test]
    fn p1_example() {
        assert_eq!(part1(EXAMPLE_INPUT), 15);
    }

    #[test]
    fn p1_input() {
        let input = std::fs::read_to_string("input/day9").expect("reading input file");
        assert_eq!(part1(&input), 562);
    }

    #[test]
    fn p2_example() {
        assert_eq!(part2(EXAMPLE_INPUT), 1134);
    }

    #[test]
    fn p2_input() {
        let input = std::fs::read_to_string("input/day9").expect("reading input file");
        assert_eq!(part2(&input), 1076922);
    }
}
