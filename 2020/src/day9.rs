use itertools::{Itertools, MinMaxResult};

fn parse(input: &str) -> impl Iterator<Item = u64> + '_ {
    input.lines().map(|l| l.parse().unwrap())
}

fn is_valid<const LEN: usize>(num: u64, buffer: &[u64; LEN]) -> bool {
    for i in 0..LEN {
        for j in (i + 1)..LEN {
            if buffer[i] + buffer[j] == num {
                return true;
            }
        }
    }
    return false;
}

fn find_first_error<I: Iterator<Item = u64>, const LEN: usize>(mut stream: I) -> u64 {
    let mut buffer = [0; LEN];
    for i in 0..LEN {
        buffer[i] = stream.next().expect("complete preamble");
    }

    let mut index = 0;
    while let Some(num) = stream.next() {
        if !is_valid(num, &buffer) {
            return num;
        }

        buffer[index] = num;
        index += 1;
        if index == LEN {
            index = 0;
        }
    }

    panic!("no error found");
}

pub fn part1(input: &str) -> u64 {
    let stream = parse(input);
    find_first_error::<_, 25>(stream)
}

fn find_weakness<const LEN: usize>(input: &str) -> u64 {
    let numbers = parse(input).collect::<Vec<_>>();

    let first_wrong = find_first_error::<_, LEN>(numbers.iter().copied());

    let (mut start, mut end) = (0, 0);
    let mut acc = numbers[0];

    for i in 1..numbers.len() {
        let num = numbers[i];
        acc += num;
        end += 1;

        while acc > first_wrong {
            acc -= numbers[start];
            start += 1;
        }

        if acc == first_wrong {
            match numbers[start..=end].iter().minmax() {
                MinMaxResult::MinMax(min, max) => return min + max,
                MinMaxResult::OneElement(_) => continue,
                MinMaxResult::NoElements => unreachable!(),
            }
        }
    }

    panic!("found no weakness!");
}

pub fn part2(input: &str) -> u64 {
    find_weakness::<25>(input)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn p1_input() {
        let input = std::fs::read_to_string("input/day9").expect("reading input file");
        assert_eq!(part1(&input), 15690279);
    }

    #[test]
    fn p1_example1() {
        assert_eq!(find_first_error::<_, 5>(parse(EXAMPLE1_INPUT)), 127);
    }

    #[test]
    fn p2_input() {
        let input = std::fs::read_to_string("input/day9").expect("reading input file");
        assert_eq!(part2(&input), 2174232);
    }

    #[test]
    fn p2_example1() {
        assert_eq!(find_weakness::<5>(EXAMPLE1_INPUT), 62);
    }

    const EXAMPLE1_INPUT: &'static str = "\
        35\n\
        20\n\
        15\n\
        25\n\
        47\n\
        40\n\
        62\n\
        55\n\
        65\n\
        95\n\
        102\n\
        117\n\
        150\n\
        182\n\
        127\n\
        219\n\
        299\n\
        277\n\
        309\n\
        567";
}
