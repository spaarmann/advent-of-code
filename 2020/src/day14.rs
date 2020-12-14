use itertools::Itertools;
use lazy_static::lazy_static;
use regex::Regex;
use std::collections::HashMap;

#[derive(Clone, Debug, Eq, PartialEq)]
enum Op {
    Mem {
        addr: u64,
        value: u64,
    },
    Mask {
        and: u64,
        or: u64,
        x_positions: Vec<usize>,
    },
}

fn parse(input: &str) -> impl Iterator<Item = Op> + '_ {
    lazy_static! {
        static ref MEM_RE: Regex = Regex::new(r"mem\[(\d+)\] = (\d+)").unwrap();
    }

    input.lines().map(|line| {
        if line.starts_with("mask = ") {
            let mask = &line[7..];
            let and = u64::from_str_radix(&mask.replace('X', "1"), 2).unwrap();
            let or = u64::from_str_radix(&mask.replace('X', "0"), 2).unwrap();
            let x_positions = mask
                .bytes()
                .positions(|c| c == b'X')
                .map(|p| 36 - p - 1)
                .collect();
            Op::Mask {
                and,
                or,
                x_positions,
            }
        } else {
            let captures = MEM_RE.captures(line).unwrap();
            let addr = captures.get(1).unwrap().as_str().parse().unwrap();
            let value = captures.get(2).unwrap().as_str().parse().unwrap();
            Op::Mem { addr, value }
        }
    })
}

pub fn part1(input: &str) -> u64 {
    let ops = parse(input);
    let mut memory = HashMap::new();

    let mut mask_and = u64::MAX;
    let mut mask_or = 0;
    for op in ops {
        match op {
            Op::Mask { and, or, .. } => {
                mask_and = and;
                mask_or = or;
            }
            Op::Mem { addr, value } => {
                memory.insert(addr, (value & mask_and) | mask_or);
            }
        }
    }

    let mut sum = 0;
    for (_, v) in memory {
        sum += v;
    }

    sum
}

pub fn part2(input: &str) -> u64 {
    let ops = parse(input);
    let mut memory = HashMap::new();

    let mut mask_or = 0;
    let mut mask_xs = Vec::new();
    for op in ops {
        match op {
            Op::Mask {
                or, x_positions, ..
            } => {
                mask_or = or;
                mask_xs = x_positions;
            }
            Op::Mem { addr, value } => {
                for floating_vals in
                    itertools::repeat_n([0, 1].iter(), mask_xs.len()).multi_cartesian_product()
                {
                    let mut addr = addr;
                    for i in 0..mask_xs.len() {
                        if *floating_vals[i] == 0 {
                            addr &= !(1 << mask_xs[i]);
                        } else {
                            addr |= 1 << mask_xs[i];
                        }
                    }

                    addr |= mask_or;
                    memory.insert(addr, value);
                }
            }
        }
    }

    let mut sum = 0;
    for (_, v) in memory {
        sum += v;
    }

    sum
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn p1_input() {
        let input = std::fs::read_to_string("input/day14").expect("reading input file");
        assert_eq!(part1(&input), 11501064782628);
    }

    #[test]
    fn p1_example1() {
        assert_eq!(part1(EXAMPLE1_INPUT), 165);
    }

    #[test]
    fn p2_input() {
        let input = std::fs::read_to_string("input/day14").expect("reading input file");
        assert_eq!(part2(&input), 5142195937660);
    }

    #[test]
    fn p2_example1() {
        assert_eq!(part2(EXAMPLE2_INPUT), 208);
    }

    const EXAMPLE1_INPUT: &'static str = "\
        mask = XXXXXXXXXXXXXXXXXXXXXXXXXXXXX1XXXX0X\n\
        mem[8] = 11\n\
        mem[7] = 101\n\
        mem[8] = 0";

    const EXAMPLE2_INPUT: &'static str = "\
        mask = 000000000000000000000000000000X1001X\n\
        mem[42] = 100\n\
        mask = 00000000000000000000000000000000X0XX\n\
        mem[26] = 1";
}
