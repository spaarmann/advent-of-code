use std::collections::VecDeque;

#[derive(Copy, Clone)]
enum Operation {
    Add(i64),
    Mul(i64),
    Square,
}

impl Operation {
    fn apply(self, num: i64) -> i64 {
        match self {
            Operation::Add(x) => num + x,
            Operation::Mul(x) => num * x,
            Operation::Square => num * num,
        }
    }
}

struct Monkey {
    op: Operation,
    test: i64,
    target_true: usize,
    target_false: usize,
}

fn parse(input: &str) -> Option<(Vec<Monkey>, Vec<VecDeque<i64>>)> {
    let mut monkeys = Vec::new();
    let mut items = Vec::new();

    for block in input.split("\n\n") {
        let mut lines = block.lines();
        lines.next(); // Skip "Monkey X:" header

        let starting_items = lines
            .next()?
            .split_once(": ")?
            .1
            .split(", ")
            .map(|n| n.parse().unwrap())
            .collect();
        items.push(starting_items);

        let op = lines.next()?.split_once(" = ").unwrap().1;
        let op = if op == "old * old" {
            Operation::Square
        } else {
            let mut tokens = op.split_whitespace();
            tokens.next();
            let operator = tokens.next()?;
            let operand = tokens.next()?.parse().unwrap();
            match operator {
                "+" => Operation::Add(operand),
                "*" => Operation::Mul(operand),
                _ => panic!("invalid operation!"),
            }
        };

        let test = lines.next()?.split_once(" by ")?.1.parse().ok()?;
        let target_true = lines.next()?.split_once(" monkey ")?.1.parse().ok()?;
        let target_false = lines.next()?.split_once(" monkey ")?.1.parse().ok()?;

        monkeys.push(Monkey {
            op,
            test,
            target_true,
            target_false,
        });
    }

    Some((monkeys, items))
}

fn run(input: &str, rounds: usize, reduce_worry: bool) -> u64 {
    let (monkeys, mut items) = parse(input).unwrap();
    let mut inspections = vec![0; monkeys.len()];

    let tests_product: i64 = monkeys.iter().map(|m| m.test).product();

    for _round in 0..rounds {
        for (i, monkey) in monkeys.iter().enumerate() {
            while let Some(mut item) = items[i].pop_front() {
                inspections[i] += 1;

                item = monkey.op.apply(item);
                if reduce_worry {
                    item = item / 3;
                }
                item = item % tests_product;

                let target = if item % monkey.test == 0 {
                    monkey.target_true
                } else {
                    monkey.target_false
                };
                items[target].push_back(item);
            }
        }
    }

    inspections.select_nth_unstable_by(2, |a, b| b.cmp(a));
    inspections[0] * inspections[1]
}

pub fn part1(input: &str) -> u64 {
    run(input, 20, true)
}

pub fn part2(input: &str) -> u64 {
    run(input, 10000, false)
}

#[cfg(test)]
mod tests {
    use super::*;

    const EXAMPLE_INPUT: &'static str = "Monkey 0:
  Starting items: 79, 98
  Operation: new = old * 19
  Test: divisible by 23
    If true: throw to monkey 2
    If false: throw to monkey 3

Monkey 1:
  Starting items: 54, 65, 75, 74
  Operation: new = old + 6
  Test: divisible by 19
    If true: throw to monkey 2
    If false: throw to monkey 0

Monkey 2:
  Starting items: 79, 60, 97
  Operation: new = old * old
  Test: divisible by 13
    If true: throw to monkey 1
    If false: throw to monkey 3

Monkey 3:
  Starting items: 74
  Operation: new = old + 3
  Test: divisible by 17
    If true: throw to monkey 0
    If false: throw to monkey 1";

    #[test]
    fn p1_example() {
        assert_eq!(part1(EXAMPLE_INPUT), 10605);
    }

    #[test]
    fn p1_input() {
        let input = std::fs::read_to_string("input/day11").expect("reading input file");
        assert_eq!(part1(&input), 57348);
    }

    #[test]
    fn p2_example() {
        assert_eq!(part2(EXAMPLE_INPUT), 2713310158);
    }

    #[test]
    fn p2_input() {
        let input = std::fs::read_to_string("input/day11").expect("reading input file");
        assert_eq!(part2(&input), 14106266886);
    }
}
