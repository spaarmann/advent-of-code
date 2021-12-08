use itertools::Itertools;

pub fn part1(input: &str) -> u64 {
    input
        .lines()
        .map(|line| line.split_once('|').unwrap().1.split_whitespace())
        .flatten()
        .filter(|digits| {
            let len = digits.len();
            len == 2 || len == 3 || len == 4 || len == 7
        })
        .count() as u64
}

fn normalize_signal(signal: &str) -> u8 {
    signal.bytes().map(|c| 1 << (c - b'a')).sum()
}

pub fn part2(input: &str) -> u64 {
    let mut output_sum = 0;

    for line in input.lines() {
        let (input, output) = line.split_once('|').unwrap();
        let signals = input.split_whitespace().map(normalize_signal).collect_vec();

        // Instead of mapping each individual input "wire" to a segment (which was my first
        // approach), we can normalize the input signals (by parsing it into a bitset in this case)
        // and then just store, for each digit, which wires need to be on to turn it on.
        // I did not come up with the idea of normalizing into a bitset, so instead I stole it from
        // https://github.com/orlp/aoc2021/blob/master/src/bin/day08b.rs.
        let mut digit_to_signal = [0; 10];

        for &signal in signals.iter() {
            // First, find out which wires each of the digits with a unique amount of 'on' segments
            // is mapped to.
            match signal.count_ones() {
                2 => digit_to_signal[1] = signal,
                4 => digit_to_signal[4] = signal,
                3 => digit_to_signal[7] = signal,
                7 => digit_to_signal[8] = signal,
                _ => (),
            }
        }

        // Based on those, we can figure out the rest:
        // - 3 is the only digit requiring 5 segments where both of the segments used for 1 is turned
        //   on.
        // - 2 and 5 are the other two 5-segment digits.
        //   - 2 has one of the segments that 1 has ('c'), and two of the ones that 4 has ('c' and 'd').
        //   - 5 also has one of the segments that 1 has ('f'), but it has three of the ones that 4 has ('b', 'd', and 'f').
        // - 0, 6, and 9 all have 6 segments on.
        //   - 6 is the only one that only has partial overlap with 1 ('c' is off), the other two
        //     have both 'c' and 'f' on.
        //   - 9 has full overlap with 4, while 0 is missing 'd' and thus has only three of the
        //     segments of 4.
        for signal in signals {
            let overlap_one = (signal & digit_to_signal[1]).count_ones();
            let overlap_four = (signal & digit_to_signal[4]).count_ones();
            match (signal.count_ones(), overlap_one, overlap_four) {
                (5, 2, _) => digit_to_signal[3] = signal,
                (5, 1, 2) => digit_to_signal[2] = signal,
                (5, 1, 3) => digit_to_signal[5] = signal,
                (6, 1, _) => digit_to_signal[6] = signal,
                (6, _, 4) => digit_to_signal[9] = signal,
                (6, _, 3) => digit_to_signal[0] = signal,
                _ => (),
            }
        }

        let output_digits = output
            .split_whitespace()
            .map(normalize_signal)
            .map(|signal| digit_to_signal.iter().position(|s| *s == signal).unwrap());

        output_sum += output_digits.fold(0, |acc, d| acc * 10 + d);
    }

    output_sum as u64
}

#[cfg(test)]
mod tests {
    use super::*;

    const EXAMPLE_INPUT: &'static str =
        "be cfbegad cbdgef fgaecd cgeb fdcge agebfd fecdb fabcd edb | fdgacbe cefdb cefbgd gcbe
edbfga begcd cbg gc gcadebf fbgde acbgfd abcde gfcbed gfec | fcgedb cgb dgebacf gc
fgaebd cg bdaec gdafb agbcfd gdcbef bgcad gfac gcb cdgabef | cg cg fdcagb cbg
fbegcd cbd adcefb dageb afcb bc aefdc ecdab fgdeca fcdbega | efabcd cedba gadfec cb
aecbfdg fbg gf bafeg dbefa fcge gcbea fcaegb dgceab fcbdga | gecf egdcabf bgf bfgea
fgeab ca afcebg bdacfeg cfaedg gcfdb baec bfadeg bafgc acf | gebdcfa ecba ca fadegcb
dbcfg fgd bdegcaf fgec aegbdf ecdfab fbedc dacgb gdcebf gf | cefg dcbef fcge gbcadfe
bdfegc cbegaf gecbf dfcage bdacg ed bedf ced adcbefg gebcd | ed bcgafe cdgba cbgef
egadfb cdbfeg cegd fecab cgb gbdefca cg fgcdab egfdb bfceg | gbdfcae bgc cg cgb
gcafb gcf dcaebfg ecagb gf abcdeg gaef cafbge fdbac fegbdc | fgae cfgab fg bagce
";

    #[test]
    fn p1_example() {
        assert_eq!(part1(EXAMPLE_INPUT), 26);
    }

    #[test]
    fn p1_input() {
        let input = std::fs::read_to_string("input/day8").expect("reading input file");
        assert_eq!(part1(&input), 381);
    }

    #[test]
    fn p2_example() {
        assert_eq!(part2(EXAMPLE_INPUT), 61229);
    }

    #[test]
    fn p2_input() {
        let input = std::fs::read_to_string("input/day8").expect("reading input file");
        assert_eq!(part2(&input), 1023686);
    }
}
