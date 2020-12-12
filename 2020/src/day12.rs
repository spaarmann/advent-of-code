#[derive(Debug, Copy, Clone, Eq, PartialEq)]
enum Op {
    North,
    South,
    East,
    West,
    Forward,
    Right,
    Left,
}

use Op::*;

fn parse(input: &str) -> impl Iterator<Item = (Op, i64)> + '_ {
    input.lines().map(|line| {
        let op = match line.chars().next().unwrap() {
            'N' => North,
            'S' => South,
            'E' => East,
            'W' => West,
            'F' => Forward,
            'R' => Right,
            'L' => Left,
            _ => panic!("invalid action"),
        };
        let num = line[1..].parse().expect("invlid number");
        (op, num)
    })
}

pub fn part1(input: &str) -> u64 {
    let mut pos = (0, 0);
    // 0 is east, angle counterclockwise
    let mut dir = 0;

    for (op, num) in parse(input) {
        match op {
            North => pos.1 += num,
            South => pos.1 -= num,
            East => pos.0 += num,
            West => pos.0 -= num,
            Right => dir = (dir - num + 360) % 360,
            Left => dir = (dir + num + 360) % 360,
            Forward => match dir {
                0 => pos.0 += num,
                90 => pos.1 += num,
                180 => pos.0 -= num,
                270 => pos.1 -= num,
                _ => panic!("invalid direction reached: {}!", dir),
            },
        }
    }

    pos.0.unsigned_abs() + pos.1.unsigned_abs()
}

pub fn part2(input: &str) -> u64 {
    let mut pos = (0, 0);
    let mut waypoint = (10, 1);

    for (op, num) in parse(input) {
        match op {
            North => waypoint.1 += num,
            South => waypoint.1 -= num,
            East => waypoint.0 += num,
            West => waypoint.0 -= num,
            Forward => pos = (pos.0 + waypoint.0 * num, pos.1 + waypoint.1 * num),
            Right => {
                for _ in 0..(num / 90) {
                    waypoint = (waypoint.1, -waypoint.0);
                }
            }
            Left => {
                for _ in 0..(num / 90) {
                    waypoint = (-waypoint.1, waypoint.0);
                }
            }
        }
    }

    pos.0.unsigned_abs() + pos.1.unsigned_abs()
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn p1_input() {
        let input = std::fs::read_to_string("input/day12").expect("reading input file");
        assert_eq!(part1(&input), 2270);
    }

    #[test]
    fn p1_example1() {
        assert_eq!(part1(EXAMPLE1_INPUT), 25);
    }

    #[test]
    fn p2_input() {
        let input = std::fs::read_to_string("input/day12").expect("reading input file");
        assert_eq!(part2(&input), 138669);
    }

    #[test]
    fn p2_example1() {
        assert_eq!(part2(EXAMPLE1_INPUT), 286);
    }

    const EXAMPLE1_INPUT: &'static str = "\
        F10\n\
        N3\n\
        F7\n\
        R90\n\
        F11";
}
