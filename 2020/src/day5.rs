fn to_digit(c: char) -> u64 {
    match c {
        'F' | 'L' => 0,
        'B' | 'R' => 1,
        _ => panic!("invalid input"),
    }
}

fn parse(input: &str) -> impl Iterator<Item = u64> + '_ {
    input
        .lines()
        .map(|line| line.chars().fold(0, |v, c| (v << 1) | to_digit(c)))
}

pub fn part1(input: &str) -> u64 {
    parse(input).max().unwrap()
}

pub fn part2(input: &str) -> u64 {
    let mut seats = parse(input).collect::<Vec<_>>();
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
        assert_eq!(seats[0], 357);
        assert_eq!(seats[1], 567);
        assert_eq!(seats[2], 119);
        assert_eq!(seats[3], 820);
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
