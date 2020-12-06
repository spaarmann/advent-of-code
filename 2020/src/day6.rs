use itertools::Itertools;

pub fn part1(input: &str) -> u64 {
    input
        .split("\n\n")
        .map(|group| group.lines().flat_map(|line| line.chars()).unique().count() as u64)
        .sum()
}

pub fn part2(input: &str) -> u64 {
    input
        .split("\n\n")
        .map(|group| {
            ('a'..='z')
                .map::<u64, _>(|q| group.lines().all(|p| p.contains(q)).into())
                .sum::<u64>()
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
