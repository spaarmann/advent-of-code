fn sum_bools(bools: &[bool]) -> u64 {
    bools.iter().map(|b| u64::from(*b)).sum()
}

pub fn part1(input: &str) -> u64 {
    input
        .split("\n\n")
        .map(|group_input| {
            let mut answers = [false; 26];
            group_input.chars().for_each(|c| {
                if let 'a'..='z' = c {
                    answers[(c as usize) - ('a' as usize)] = true;
                }
            });
            sum_bools(&answers)
        })
        .sum()
}

pub fn part2(input: &str) -> u64 {
    input
        .split("\n\n")
        .map(|group_input| {
            let mut answers = [true; 26];
            group_input.lines().for_each(|line| {
                for c in 'a'..='z' {
                    if !line.contains(c) {
                        answers[(c as usize) - ('a' as usize)] = false;
                    }
                }
            });
            sum_bools(&answers)
        })
        .sum()
}

#[cfg(test)]
mod tests {
    use super::*;

    const EXAMPLE1_INPUT: &'static str = "abc\n\
    \n\
                                          a\n\
                                          b\n\
                                          c\n\
    \n\
                                          ab\n\
                                          ac\n\
    \n\
                                          a\n\
                                          a\n\
                                          a\n\
                                          a\n\
    \n\
                                          b";

    #[test]
    fn p1_input() {
        let input = std::fs::read_to_string("input/day6").expect("reading input file");
        assert_eq!(part1(&input), 6249);
    }

    #[test]
    fn p1_example1() {
        assert_eq!(part1(EXAMPLE1_INPUT), 11);
    }

    #[test]
    fn p2_input() {
        let input = std::fs::read_to_string("input/day6").expect("reading input file");
        assert_eq!(part2(&input), 3103);
    }

    #[test]
    fn p2_example1() {
        assert_eq!(part2(EXAMPLE1_INPUT), 6);
    }
}
