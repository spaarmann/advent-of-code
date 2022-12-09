use itertools::Itertools;

fn get_head_positions(input: &str) -> impl Iterator<Item = (i64, i64)> + '_ {
    fn parse_move(line: &str) -> ((i64, i64), usize) {
        let (dir, count) = line.split_once(' ').unwrap();
        let dir = match dir {
            "R" => (1, 0),
            "L" => (-1, 0),
            "U" => (0, 1),
            "D" => (0, -1),
            _ => panic!("invalid move direction"),
        };
        (dir, count.parse().unwrap())
    }

    input
        .lines()
        .flat_map(|l| {
            let (dir, count) = parse_move(l);
            std::iter::repeat(dir).take(count)
        })
        .scan((0, 0), |pos, mov| {
            pos.0 += mov.0;
            pos.1 += mov.1;
            Some(*pos)
        })
}

fn get_new_tail_position(tail: (i64, i64), head: (i64, i64)) -> (i64, i64) {
    // tail + diff = head
    let diff = (head.0 - tail.0, head.1 - tail.1);
    let tail_move = match diff {
        // Overlapping or touching
        (0, 0) | (1, 0) | (-1, 0) | (0, 1) | (0, -1) | (1, 1) | (1, -1) | (-1, 1) | (-1, -1) => {
            (0, 0)
        }
        // In same row or column
        (2, 0) => (1, 0),
        (-2, 0) => (-1, 0),
        (0, 2) => (0, 1),
        (0, -2) => (0, -1),
        // Diagonal moves
        (1, 2) | (2, 1) | (2, 2) => (1, 1),
        (1, -2) | (2, -1) | (2, -2) => (1, -1),
        (-1, 2) | (-2, 1) | (-2, 2) => (-1, 1),
        (-1, -2) | (-2, -1) | (-2, -2) => (-1, -1),
        _ => panic!(
            "tail can't keep up! tail is {:?} and head is {:?}",
            tail, head
        ),
    };

    (tail.0 + tail_move.0, tail.1 + tail_move.1)
}

pub fn part1(input: &str) -> u64 {
    get_head_positions(input)
        .scan((0, 0), |tail, head| {
            *tail = get_new_tail_position(*tail, head);
            Some(*tail)
        })
        .unique()
        .count() as u64
}

pub fn part2(input: &str) -> u64 {
    get_head_positions(input)
        .scan([(0, 0); 9], |knots, head| {
            knots[0] = get_new_tail_position(knots[0], head);
            for i in 0..8 {
                knots[i + 1] = get_new_tail_position(knots[i + 1], knots[i]);
            }
            Some(knots[8])
        })
        .unique()
        .count() as u64
}

#[cfg(test)]
mod tests {
    use super::*;

    const EXAMPLE_INPUT: &'static str = "R 4
U 4
L 3
D 1
R 4
D 1
L 5
R 2";

    #[test]
    fn p1_example() {
        assert_eq!(part1(EXAMPLE_INPUT), 13);
    }

    #[test]
    fn p1_input() {
        let input = std::fs::read_to_string("input/day9").expect("reading input file");
        assert_eq!(part1(&input), 6243);
    }

    #[test]
    fn p2_example() {
        assert_eq!(part2(EXAMPLE_INPUT), 1);
    }

    const PART2_EXAMPLE2: &'static str = "R 5
U 8
L 8
D 3
R 17
D 10
L 25
U 20";

    #[test]
    fn p2_example2() {
        assert_eq!(part2(PART2_EXAMPLE2), 36);
    }

    #[test]
    fn p2_input() {
        let input = std::fs::read_to_string("input/day9").expect("reading input file");
        assert_eq!(part2(&input), 2630);
    }
}
