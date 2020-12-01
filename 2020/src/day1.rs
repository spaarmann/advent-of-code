fn parse(input: &str) -> Vec<u64> {
    let mut numbers = Vec::new();
    for line in input.lines() {
        numbers.push(line.parse::<u64>().expect("lines only contain a number!"));
    }
    numbers
}

pub fn part1(input: &str) -> u64 {
    let numbers = parse(input);

    for a in &numbers {
        for b in &numbers {
            if a + b == 2020 {
                return a * b;
            }
        }
    }

    panic!("no numbers adding to 2020 found!");
}

pub fn part2(input: &str) -> u64 {
    let numbers = parse(input);

    for a in &numbers {
        for b in &numbers {
            for c in &numbers {
                if a + b + c == 2020 {
                    return a * b * c;
                }
            }
        }
    }

    panic!("no numbers adding to 2020 found!");
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn day1_p1_example1() {
        let input = "1721\n979\n366\n299\n675\n1456";
        assert_eq!(part1(input), 514579);
    }

    #[test]
    fn day1_p2_example1() {
        let input = "1721\n979\n366\n299\n675\n1456";
        assert_eq!(part2(input), 241861950);
    }
}
