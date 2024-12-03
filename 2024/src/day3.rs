use regex::Regex;

pub fn part1(input: &str) -> u64 {
    let re = Regex::new(r#"mul\((\d{1,3}),(\d{1,3})\)"#).unwrap();
    re.captures_iter(input)
        .map(|captures| {
            let c1 = captures.get(1).unwrap();
            let c2 = captures.get(2).unwrap();
            c1.as_str().parse::<u64>().unwrap() * c2.as_str().parse::<u64>().unwrap()
        })
        .sum()
}

pub fn part2(input: &str) -> u64 {
    let re = Regex::new(r#"(do\(\))|(don't\(\))|mul\((\d{1,3}),(\d{1,3})\)"#).unwrap();
    let mut active = true;
    re.captures_iter(input)
        .map(|captures| {
            if captures.get(1).is_some() {
                active = true;
                0
            } else if captures.get(2).is_some() {
                active = false;
                0
            } else if active {
                let c1 = captures.get(3).unwrap();
                let c2 = captures.get(4).unwrap();
                c1.as_str().parse::<u64>().unwrap() * c2.as_str().parse::<u64>().unwrap()
            } else {
                0
            }
        })
        .sum()
}

#[cfg(test)]
mod tests {
    use super::*;

    const EXAMPLE_INPUT: &'static str =
        "xmul(2,4)%&mul[3,7]!@^do_not_mul(5,5)+mul(32,64]then(mul(11,8)mul(8,5))";
    const EXAMPLE2_INPUT: &'static str =
        "xmul(2,4)&mul[3,7]!^don't()_mul(5,5)+mul(32,64](mul(11,8)undo()?mul(8,5))";

    #[test]
    fn p1_example() {
        assert_eq!(part1(EXAMPLE_INPUT), 161);
    }

    #[test]
    fn p1_input() {
        let input = std::fs::read_to_string("input/day3").expect("reading input file");
        assert_eq!(part1(&input), 171183089);
    }

    #[test]
    fn p2_example() {
        assert_eq!(part2(EXAMPLE2_INPUT), 48);
    }

    #[test]
    fn p2_input() {
        let input = std::fs::read_to_string("input/day3").expect("reading input file");
        assert_eq!(part2(&input), 63866497);
    }
}
