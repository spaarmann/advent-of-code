fn compute(races: impl Iterator<Item = (i64, i64)>) -> u64 {
    races
        .map(|(t, r)| {
            // If b is the time the button is held, then the resulting distance d is
            //   d = b * (t - b) = -b^2 + tb
            // This is a quadratic function d(b) that will have two intersection points with the line
            // given by d'(b) = r and be better than the record exactly between those.
            //   r = -b^2 + tb
            //   0 = -b^2 + tb - r
            //   0 = b^2 - tb + r
            // =>
            //   b{1,2} = t/2 +- sqrt((-t/2)^2 - r)
            let (t, r) = (t as f64, r as f64);
            let b1 = t * 0.5 + ((-t * 0.5).powi(2) - r).sqrt();
            let b2 = t * 0.5 - ((-t * 0.5).powi(2) - r).sqrt();

            let b1 = (b1 - 0.001).floor() as i64;
            let b2 = (b2 + 0.001).ceil() as i64;

            b1.abs_diff(b2) + 1
        })
        .product::<u64>()
}

pub fn part1(input: &str) -> u64 {
    let mut parsed_lines = input.lines().map(|l| {
        l.split_once(":")
            .unwrap()
            .1
            .split_whitespace()
            .map(|n| n.parse::<i64>().unwrap())
    });
    let times = parsed_lines.next().unwrap();
    let records = parsed_lines.next().unwrap();

    compute(times.zip(records))
}

pub fn part2(input: &str) -> u64 {
    let mut parsed_lines = input.lines().map(|l| {
        l.split_once(":")
            .unwrap()
            .1
            .chars()
            .filter(char::is_ascii_digit)
            .collect::<String>()
            .parse::<i64>()
            .unwrap()
    });
    let time = parsed_lines.next().unwrap();
    let record = parsed_lines.next().unwrap();

    compute(std::iter::once((time, record)))
}

#[cfg(test)]
mod tests {
    use super::*;

    const EXAMPLE_INPUT: &'static str = "Time:      7  15   30
Distance:  9  40  200
";

    #[test]
    fn p1_example() {
        assert_eq!(part1(EXAMPLE_INPUT), 288);
    }

    #[test]
    fn p1_input() {
        let input = std::fs::read_to_string("input/day6").expect("reading input file");
        assert_eq!(part1(&input), 503424);
    }

    #[test]
    fn p2_example() {
        assert_eq!(part2(EXAMPLE_INPUT), 71503);
    }

    #[test]
    fn p2_input() {
        let input = std::fs::read_to_string("input/day6").expect("reading input file");
        assert_eq!(part2(&input), 32607562);
    }
}
