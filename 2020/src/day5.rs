fn parse(input: &str) -> impl Iterator<Item = (u64, u64, u64)> + '_ {
    input.lines().map(|line| {
        let mut chars = line.chars();
        let mut row_range = (0, 128); // [0, 128)

        for _ in 0..7 {
            match chars.next().unwrap() {
                'F' => row_range.1 -= (row_range.1 - row_range.0) >> 1,
                'B' => row_range.0 += (row_range.1 - row_range.0) >> 1,
                _ => panic!("invalid input"),
            }
        }

        let mut col_range = (0, 8);
        for _ in 0..3 {
            match chars.next().unwrap() {
                'L' => col_range.1 -= (col_range.1 - col_range.0) >> 1,
                'R' => col_range.0 += (col_range.1 - col_range.0) >> 1,
                _ => panic!("invlid input"),
            }
        }

        (row_range.0, col_range.0, row_range.0 * 8 + col_range.0)
    })
}

pub fn part1(input: &str) -> u64 {
    parse(input).map(|seat| seat.2).max().unwrap()
}

pub fn part2(input: &str) -> u64 {
    let mut seats = parse(input).map(|seat| seat.2).collect::<Vec<_>>();
    seats.sort_unstable();
    for i in 1..seats.len() {
        if seats[i - 1] + 2 == seats[i] {
            return seats[i] - 1;
        }
    }

    panic!("no free seat found");
}

#[cfg(test)]
mod tests {
    use super::*;

    const EXAMPLE1_INPUT: &'static str = "FBFBBFFRLR\n\
                                          BFFFBBFRRR\n\
                                          FFFBBBFRRR\n\
                                          BBFFBBFRLL";

    #[test]
    fn p1_input() {
        let input = std::fs::read_to_string("input/day5").expect("reading input file");
        assert_eq!(part1(&input), 994);
    }

    #[test]
    fn p1_example1() {
        let seats = parse(EXAMPLE1_INPUT).collect::<Vec<_>>();
        assert_eq!(seats[0], (44, 5, 357));
        assert_eq!(seats[1], (70, 7, 567));
        assert_eq!(seats[2], (14, 7, 119));
        assert_eq!(seats[3], (102, 4, 820));
        assert_eq!(part1(EXAMPLE1_INPUT), 820);
    }

    #[test]
    fn p2_input() {
        let input = std::fs::read_to_string("input/day5").expect("reading input file");
        assert_eq!(part2(&input), 741);
    }

    /*#[test]
    fn p2_example1() {
        assert_eq!(part2(EXAMPLE1_INPUT), 336);
    }*/
}
