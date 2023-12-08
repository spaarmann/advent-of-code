use itertools::Itertools;
use lazy_static::lazy_static;
use regex::Regex;
use std::collections::HashMap;

type Node<'a> = &'a str;
type Network<'a> = HashMap<Node<'a>, (Node<'a>, Node<'a>)>;

enum Instr {
    Left,
    Right,
}
impl Instr {
    fn from_char(c: char) -> Self {
        match c {
            'L' => Instr::Left,
            'R' => Instr::Right,
            _ => panic!("invalid isntruction"),
        }
    }
}

fn parse(input: &str) -> (Vec<Instr>, Network<'_>) {
    lazy_static! {
        static ref NODE_RE: Regex = Regex::new(r"(...) = \((...), (...)\)").unwrap();
    }

    let (instrs, network) = input.split_once("\n\n").unwrap();
    let instrs = instrs.chars().map(Instr::from_char).collect();

    let network = network
        .lines()
        .map(|l| {
            let captures = NODE_RE.captures(l).unwrap();
            let name = captures.get(1).unwrap().as_str();
            let left_edge = captures.get(2).unwrap().as_str();
            let right_edge = captures.get(3).unwrap().as_str();
            (name, (left_edge, right_edge))
        })
        .collect();

    (instrs, network)
}

pub fn part1(input: &str) -> u64 {
    let (instrs, network) = parse(input);

    let mut node = "AAA";
    let mut steps = 0;
    for instr in instrs.iter().cycle() {
        let edges = network[node];
        node = match instr {
            Instr::Left => edges.0,
            Instr::Right => edges.1,
        };
        steps += 1;

        if node == "ZZZ" {
            break;
        }
    }

    steps
}

pub fn part2(input: &str) -> u64 {
    let (instrs, network) = parse(input);

    let mut nodes = network
        .keys()
        .copied()
        .filter(|n| n.ends_with('A'))
        .collect_vec();

    let mut first_end = vec![None; nodes.len()];
    let mut loop_lens = vec![None; nodes.len()];

    let mut steps = 0;
    for instr in instrs.iter().cycle() {
        steps += 1;

        for (i, node) in nodes.iter_mut().enumerate() {
            let edges = network[*node];
            *node = match instr {
                Instr::Left => edges.0,
                Instr::Right => edges.1,
            };

            if node.ends_with('Z') {
                match first_end[i] {
                    None => first_end[i] = Some((*node, steps)),
                    Some((n, loop_start)) if n == *node && loop_lens[i].is_none() => {
                        loop_lens[i] = Some(steps - loop_start)
                    }
                    _ => (),
                };
            }
        }

        if loop_lens.iter().all(Option::is_some) {
            break;
        }
    }

    let loop_lcm = loop_lens
        .into_iter()
        .map(Option::unwrap)
        .map(|n| n as u128)
        .reduce(num::integer::lcm)
        .unwrap();

    //steps
    loop_lcm as u64
}

#[cfg(test)]
mod tests {
    use super::*;

    const EXAMPLE_INPUT: &'static str = "RL

AAA = (BBB, CCC)
BBB = (DDD, EEE)
CCC = (ZZZ, GGG)
DDD = (DDD, DDD)
EEE = (EEE, EEE)
GGG = (GGG, GGG)
ZZZ = (ZZZ, ZZZ)
";

    #[test]
    fn p1_example() {
        assert_eq!(part1(EXAMPLE_INPUT), 2);
    }

    const EXAMPLE_INPUT2: &'static str = "LLR

AAA = (BBB, BBB)
BBB = (AAA, ZZZ)
ZZZ = (ZZZ, ZZZ)
";

    #[test]
    fn p1_example2() {
        assert_eq!(part1(EXAMPLE_INPUT2), 6);
    }

    #[test]
    fn p1_input() {
        let input = std::fs::read_to_string("input/day8").expect("reading input file");
        assert_eq!(part1(&input), 20777);
    }

    const EXAMPLE_INPUT3: &'static str = "LR

11A = (11B, XXX)
11B = (XXX, 11Z)
11Z = (11B, XXX)
22A = (22B, XXX)
22B = (22C, 22C)
22C = (22Z, 22Z)
22Z = (22B, 22B)
XXX = (XXX, XXX)
";

    #[test]
    fn p2_example() {
        assert_eq!(part2(EXAMPLE_INPUT3), 6);
    }

    #[test]
    fn p2_input() {
        let input = std::fs::read_to_string("input/day8").expect("reading input file");
        assert_eq!(part2(&input), 13289612809129);
    }
}
