use std::cmp::Ordering;

use itertools::Itertools;

#[derive(PartialEq, Eq, Debug, Clone)]
enum Value {
    Integer(i64),
    List(Vec<Value>),
}

impl Value {
    fn from_str(s: &str) -> Self {
        let mut stack = Vec::new();
        let mut current_num = None;
        for c in s.chars() {
            match c {
                '[' => stack.push(Vec::new()),
                ']' => {
                    if let Some(num) = current_num {
                        stack.last_mut().unwrap().push(Value::Integer(num));
                        current_num = None;
                    }

                    if stack.len() > 1 {
                        // Don't pop the last one, we return that below the loop
                        let last = stack.pop().unwrap();
                        stack.last_mut().unwrap().push(Value::List(last));
                    }
                }
                c if c.is_digit(10) => {
                    let num = current_num.unwrap_or(0);
                    current_num = Some(num * 10 + c.to_digit(10).unwrap() as i64);
                }
                ',' => {
                    if let Some(num) = current_num {
                        stack.last_mut().unwrap().push(Value::Integer(num));
                        current_num = None;
                    }
                }
                _ => panic!("Unknown character in Value literal: {}", c),
            }
        }

        assert!(stack.len() == 1);
        Value::List(stack.pop().unwrap())
    }
}

impl PartialOrd<Value> for Value {
    fn partial_cmp(&self, other: &Value) -> Option<Ordering> {
        use Value::*;

        match (self, other) {
            (Integer(l), Integer(r)) => l.partial_cmp(r),
            (List(l), List(r)) => l.partial_cmp(r),
            (Integer(l), List(r)) => vec![Integer(*l)].partial_cmp(r),
            (List(l), Integer(r)) => l.partial_cmp(&vec![Integer(*r)]),
        }
    }
}

impl Ord for Value {
    fn cmp(&self, other: &Self) -> Ordering {
        self.partial_cmp(other).unwrap()
    }
}

fn parse(input: &str) -> impl Iterator<Item = Value> + '_ {
    input.lines().filter(|l| !l.is_empty()).map(Value::from_str)
}

pub fn part1(input: &str) -> u64 {
    parse(input)
        .array_chunks()
        .map(|[l, r]| (l, r))
        .enumerate()
        .filter(|(_i, (l, r))| l < r)
        .map(|(i, _)| i + 1)
        .sum::<usize>() as u64
}

pub fn part2(input: &str) -> u64 {
    let mut packets = parse(input).collect_vec();
    let div1 = Value::List(vec![Value::List(vec![Value::Integer(2)])]);
    let div2 = Value::List(vec![Value::List(vec![Value::Integer(6)])]);
    packets.push(div1.clone());
    packets.push(div2.clone());
    packets.sort();
    let idx1 = packets.iter().position(|p| p == &div1).unwrap();
    let idx2 = packets.iter().position(|p| p == &div2).unwrap();
    ((idx1 + 1) * (idx2 + 1)) as u64
}

#[cfg(test)]
mod tests {
    use super::*;

    const EXAMPLE_INPUT: &'static str = "[1,1,3,1,1]
[1,1,5,1,1]

[[1],[2,3,4]]
[[1],4]

[9]
[[8,7,6]]

[[4,4],4,4]
[[4,4],4,4,4]

[7,7,7,7]
[7,7,7]

[]
[3]

[[[]]]
[[]]

[1,[2,[3,[4,[5,6,7]]]],8,9]
[1,[2,[3,[4,[5,6,0]]]],8,9]";

    #[test]
    fn p1_example() {
        assert_eq!(part1(EXAMPLE_INPUT), 13);
    }

    #[test]
    fn p1_input() {
        let input = std::fs::read_to_string("input/day13").expect("reading input file");
        assert_eq!(part1(&input), 5390);
    }

    #[test]
    fn p2_example() {
        assert_eq!(part2(EXAMPLE_INPUT), 140);
    }

    #[test]
    fn p2_input() {
        let input = std::fs::read_to_string("input/day13").expect("reading input file");
        assert_eq!(part2(&input), 19261);
    }
}
