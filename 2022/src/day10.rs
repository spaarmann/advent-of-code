use itertools::Itertools;

fn register_values(input: &str) -> impl Iterator<Item = i64> + '_ {
    std::iter::once(0)
        .chain(input.lines().flat_map(|l| {
            if l == "noop" {
                vec![0]
            } else {
                let v = l.split_once(' ').unwrap().1.parse().unwrap();
                vec![0, v] // TODO: Having to allocate here is really annoying
            }
        }))
        .scan(1, |x, v| {
            *x += v;
            Some(*x)
        })
}

pub fn part1(input: &str) -> i64 {
    let values = register_values(input).collect_vec();

    let mut total_strength = 0;
    for cycle in [20, 60, 100, 140, 180, 220] {
        let x = values[cycle - 1];
        total_strength += cycle as i64 * x;
    }

    total_strength
}

pub fn part2(input: &str) -> String {
    std::iter::once('\n')
        .chain(
            register_values(input)
                .take(6 * 40)
                .chunks(40)
                .into_iter()
                .flat_map(|line_values| {
                    line_values
                        .enumerate()
                        .map(|(crt_pos, x)| {
                            if (crt_pos as i64).abs_diff(x) < 2 {
                                '#'
                            } else {
                                '.'
                            }
                        })
                        .chain(std::iter::once('\n'))
                }),
        )
        .collect()
}

#[cfg(test)]
mod tests {
    use super::*;

    const EXAMPLE_INPUT: &'static str = "addx 15
addx -11
addx 6
addx -3
addx 5
addx -1
addx -8
addx 13
addx 4
noop
addx -1
addx 5
addx -1
addx 5
addx -1
addx 5
addx -1
addx 5
addx -1
addx -35
addx 1
addx 24
addx -19
addx 1
addx 16
addx -11
noop
noop
addx 21
addx -15
noop
noop
addx -3
addx 9
addx 1
addx -3
addx 8
addx 1
addx 5
noop
noop
noop
noop
noop
addx -36
noop
addx 1
addx 7
noop
noop
noop
addx 2
addx 6
noop
noop
noop
noop
noop
addx 1
noop
noop
addx 7
addx 1
noop
addx -13
addx 13
addx 7
noop
addx 1
addx -33
noop
noop
noop
addx 2
noop
noop
noop
addx 8
noop
addx -1
addx 2
addx 1
noop
addx 17
addx -9
addx 1
addx 1
addx -3
addx 11
noop
noop
addx 1
noop
addx 1
noop
noop
addx -13
addx -19
addx 1
addx 3
addx 26
addx -30
addx 12
addx -1
addx 3
addx 1
noop
noop
noop
addx -9
addx 18
addx 1
addx 2
noop
noop
addx 9
noop
noop
noop
addx -1
addx 2
addx -37
addx 1
addx 3
noop
addx 15
addx -21
addx 22
addx -6
addx 1
noop
addx 2
addx 1
noop
addx -10
noop
noop
addx 20
addx 1
addx 2
addx 2
addx -6
addx -11
noop
noop
noop";

    #[test]
    fn p1_example() {
        assert_eq!(part1(EXAMPLE_INPUT), 13140);
    }

    #[test]
    fn p1_input() {
        let input = std::fs::read_to_string("input/day10").expect("reading input file");
        assert_eq!(part1(&input), 15120);
    }

    #[test]
    fn p2_example() {
        assert_eq!(
            part2(EXAMPLE_INPUT),
            "
##..##..##..##..##..##..##..##..##..##..
###...###...###...###...###...###...###.
####....####....####....####....####....
#####.....#####.....#####.....#####.....
######......######......######......####
#######.......#######.......#######.....
"
        );
    }

    #[test]
    fn p2_input() {
        let input = std::fs::read_to_string("input/day10").expect("reading input file");
        assert_eq!(
            part2(&input),
            "
###..#..#.###....##.###..###..#.....##..
#..#.#.#..#..#....#.#..#.#..#.#....#..#.
#..#.##...#..#....#.###..#..#.#....#..#.
###..#.#..###.....#.#..#.###..#....####.
#.#..#.#..#....#..#.#..#.#....#....#..#.
#..#.#..#.#.....##..###..#....####.#..#.
"
        );
    }
}
