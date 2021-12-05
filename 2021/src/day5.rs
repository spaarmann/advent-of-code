use std::str::FromStr;

#[derive(Debug, Copy, Clone)]
struct Point {
    x: i64,
    y: i64,
}

impl FromStr for Point {
    type Err = ();
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let (x, y) = s.split_once(",").unwrap();
        Ok(Point {
            x: x.parse().unwrap(),
            y: y.parse().unwrap(),
        })
    }
}

#[derive(Debug, Copy, Clone)]
struct Line {
    start: Point,
    end: Point,
}

impl FromStr for Line {
    type Err = ();

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let (start, end) = s.split_once(" -> ").unwrap();
        let (start, end) = (start.parse().unwrap(), end.parse().unwrap());

        Ok(Line { start, end })
    }
}

impl Line {
    fn iter(&self) -> impl Iterator<Item = Point> + '_ {
        let mut current = self.start;

        let delta_x = if self.start.x <= self.end.x { 1 } else { -1 };
        let delta_y = if self.start.y <= self.end.y { 1 } else { -1 };

        let mut done = false;

        std::iter::from_fn(move || {
            if done {
                return None;
            }

            if current.x == self.end.x && current.y == self.end.y {
                done = true;
                return Some(current);
            }

            let to_yield = current;

            if current.x != self.end.x {
                current.x += delta_x;
            }
            if current.y != self.end.y {
                current.y += delta_y;
            }

            Some(to_yield)
        })
    }
}

const BOARD_SIZE: i64 = 1000;

fn make_map(lines: impl Iterator<Item = Line>) -> Vec<i32> {
    let mut map = vec![0; (BOARD_SIZE * BOARD_SIZE) as usize];
    for line in lines {
        for point in line.iter() {
            map[(point.y * BOARD_SIZE + point.x) as usize] += 1;
        }
    }
    map
}

pub fn part1(input: &str) -> u64 {
    make_map(
        input
            .lines()
            .map(|l| l.parse::<Line>().unwrap())
            .filter(|l| l.start.x == l.end.x || l.start.y == l.end.y),
    )
    .iter()
    .filter(|&&n| n >= 2)
    .count() as u64
}

pub fn part2(input: &str) -> u64 {
    make_map(input.lines().map(|l| l.parse::<Line>().unwrap()))
        .iter()
        .filter(|&&n| n >= 2)
        .count() as u64
}

fn _print_map(map: &[i32]) {
    for y in 0..BOARD_SIZE {
        for x in 0..BOARD_SIZE {
            let value = map[(y * BOARD_SIZE + x) as usize];
            if value > 0 {
                print!("{}", value);
            } else {
                print!(".");
            }
        }
        println!();
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    const EXAMPLE_INPUT: &'static str = "0,9 -> 5,9
8,0 -> 0,8
9,4 -> 3,4
2,2 -> 2,1
7,0 -> 7,4
6,4 -> 2,0
0,9 -> 2,9
3,4 -> 1,4
0,0 -> 8,8
5,5 -> 8,2
";

    #[test]
    fn p1_example() {
        assert_eq!(part1(EXAMPLE_INPUT), 5);
    }

    #[test]
    fn p1_input() {
        let input = std::fs::read_to_string("input/day5").expect("reading input file");
        assert_eq!(part1(&input), 5124);
    }

    #[test]
    fn p2_example() {
        assert_eq!(part2(EXAMPLE_INPUT), 12);
    }

    #[test]
    fn p2_input() {
        let input = std::fs::read_to_string("input/day5").expect("reading input file");
        assert_eq!(part2(&input), 19771);
    }
}
