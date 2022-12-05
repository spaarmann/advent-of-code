use itertools::Itertools;
use lazy_static::lazy_static;
use regex::Regex;

#[derive(Copy, Clone, Debug)]
struct Instruction {
    from: usize,
    to: usize,
    count: usize,
}

fn parse(input: &str) -> (Vec<Vec<char>>, Vec<Instruction>) {
    lazy_static! {
        static ref INSTR_RE: Regex = Regex::new(r"move (\d+) from (\d) to (\d)").unwrap();
    }

    let mut stacks = Vec::new();
    let mut instructions = Vec::new();

    for line in input.lines() {
        if line.trim_start().starts_with('[') {
            let parts = line.chars().chunks(4);
            for (i, mut p) in parts.into_iter().enumerate() {
                if i >= stacks.len() {
                    stacks.push(Vec::new());
                }
                let stack = &mut stacks[i];

                p.next(); // skip opening [
                if let Some(krate) = p.next() && krate != ' ' {
                    stack.push(krate);
                }
            }
        } else if line.starts_with("move") {
            let captures = INSTR_RE.captures(line).unwrap();
            instructions.push(Instruction {
                count: captures.get(1).unwrap().as_str().parse().unwrap(),
                from: captures.get(2).unwrap().as_str().parse().unwrap(),
                to: captures.get(3).unwrap().as_str().parse().unwrap(),
            });
        }
    }

    for stack in &mut stacks {
        stack.reverse();
    }

    (stacks, instructions)
}

pub fn part1(input: &str) -> String {
    let (mut stacks, instructions) = parse(input);

    for instr in instructions {
        let [source, dest] = stacks.get_many_mut([instr.from - 1, instr.to - 1]).unwrap();
        for _ in 0..instr.count {
            dest.push(source.pop().unwrap());
        }
    }

    stacks.into_iter().map(|s| *s.last().unwrap()).collect()
}

pub fn part2(input: &str) -> String {
    let (mut stacks, instructions) = parse(input);

    for instr in instructions {
        let [source, dest] = stacks.get_many_mut([instr.from - 1, instr.to - 1]).unwrap();
        dest.extend(source.drain(source.len() - instr.count..));
    }

    stacks.into_iter().map(|s| *s.last().unwrap()).collect()
}

#[cfg(test)]
mod tests {
    use super::*;

    const EXAMPLE_INPUT: &'static str = "    [D]    
[N] [C]    
[Z] [M] [P]
 1   2   3 

move 1 from 2 to 1
move 3 from 1 to 3
move 2 from 2 to 1
move 1 from 1 to 2";

    #[test]
    fn p1_example() {
        assert_eq!(part1(EXAMPLE_INPUT), "CMZ");
    }

    #[test]
    fn p1_input() {
        let input = std::fs::read_to_string("input/day5").expect("reading input file");
        assert_eq!(part1(&input), "QGTHFZBHV");
    }

    #[test]
    fn p2_example() {
        assert_eq!(part2(EXAMPLE_INPUT), "MCD");
    }

    #[test]
    fn p2_input() {
        let input = std::fs::read_to_string("input/day5").expect("reading input file");
        assert_eq!(part2(&input), "MGDMPSZTM");
    }
}
