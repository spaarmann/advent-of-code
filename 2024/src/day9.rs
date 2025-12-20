use itertools::Itertools;
use num::Integer;

pub fn part1(input: &str) -> u64 {
    let mut disk = input
        .trim()
        .chars()
        .map(|c| c.to_digit(10).unwrap() as u64)
        .collect_vec();
    let mut checksum = 0;

    let mut end = disk.len() - 1;
    let mut i = 0;
    let mut block = 0;
    while i <= end {
        let len = disk[i];
        if i.is_even() {
            for o in 0..len {
                checksum += (block + o) * (i / 2) as u64;
                println!("block {} gets file {}", block + o, i / 2);
            }
            block += len;
        } else {
            for o in 0..len {
                while disk[end] == 0 {
                    end -= 2;
                }

                checksum += (block + o) * (end / 2) as u64;
                println!("block {} gets file {}", block + o, end / 2);
                disk[end] -= 1;
                //println!("{} blocks remaining for file {}", disk[end], end / 2);
            }
            block += len;
        }
        i += 1;
    }

    checksum as u64
}

pub fn part2(input: &str) -> u64 {
    todo!()
}

#[cfg(test)]
mod tests {
    use super::*;

    const EXAMPLE_INPUT: &'static str = "2333133121414131402";

    #[test]
    fn p1_example() {
        assert_eq!(part1(EXAMPLE_INPUT), 1928);
    }

    #[test]
    fn p1_input() {
        let input = std::fs::read_to_string("input/day9").expect("reading input file");
        assert_eq!(part1(&input), todo!());
    }

    #[test]
    fn p2_example() {
        assert_eq!(part2(EXAMPLE_INPUT), todo!());
    }

    #[test]
    fn p2_input() {
        let input = std::fs::read_to_string("input/day9").expect("reading input file");
        assert_eq!(part2(&input), todo!());
    }
}
