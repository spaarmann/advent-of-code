use lazy_static::lazy_static;
use regex::Regex;

#[derive(Copy, Clone, Debug)]
struct Policy {
    first: u64,
    second: u64,
    c: char,
}

fn parse(input: &str) -> impl Iterator<Item = (Policy, &str)> {
    lazy_static! {
        static ref RE: Regex = Regex::new(r"(\d+)-(\d+) ([a-z]): ([a-z]+)").unwrap();
    }

    input.lines().map(|line| {
        let captures = RE.captures(line).expect("correct input format");
        let policy = Policy {
            first: captures.get(1).unwrap().as_str().parse().unwrap(),
            second: captures.get(2).unwrap().as_str().parse().unwrap(),
            c: captures.get(3).unwrap().as_str().chars().next().unwrap(),
        };

        (policy, captures.get(4).unwrap().as_str())
    })
}

pub fn part1(input: &str) -> u64 {
    let passwords = parse(input);

    let mut valid_count = 0;
    for (policy, password) in passwords {
        let count = password.chars().filter(|c| *c == policy.c).count() as u64;
        if count >= policy.first && count <= policy.second {
            valid_count += 1;
        }
    }

    valid_count
}

pub fn part2(input: &str) -> u64 {
    let passwords = parse(input);

    let mut valid_count = 0;
    for (policy, password) in passwords {
        // The regex above is only going to match [a-z], so we can be sure to have ASCII only here.
        let password = password.as_bytes();
        let first_match = password[(policy.first - 1) as usize] as char == policy.c;
        let second_match = password[(policy.second - 1) as usize] as char == policy.c;
        if first_match ^ second_match {
            valid_count += 1;
        }
    }

    valid_count
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn p1_input() {
        let input = std::fs::read_to_string("input/day2").expect("reading input file");
        assert_eq!(part1(&input), 586);
    }

    #[test]
    fn p1_example1() {
        let input = "1-3 a: abcde\n1-3 b: cdefg\n2-9 c: ccccccccc";
        assert_eq!(part1(input), 2);
    }

    #[test]
    fn p2_input() {
        let input = std::fs::read_to_string("input/day2").expect("reading input file");
        assert_eq!(part2(&input), 352);
    }

    #[test]
    fn p2_example1() {
        let input = "1-3 a: abcde\n1-3 b: cdefg\n2-9 c: ccccccccc";
        assert_eq!(part2(input), 1);
    }
}
