use lazy_static::lazy_static;
use regex::Regex;

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
struct Restrictions<'a> {
    name: &'a str,
    first: (u64, u64),
    second: (u64, u64),
}

struct Ticket {
    numbers: Vec<u64>,
}

impl std::str::FromStr for Ticket {
    type Err = std::num::ParseIntError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        Ok(Self {
            numbers: s.split(',').map(|n| n.parse()).collect::<Result<_, _>>()?,
        })
    }
}

fn parse(input: &str) -> (Vec<Restrictions>, Ticket, impl Iterator<Item = Ticket> + '_) {
    lazy_static! {
        static ref RE_RESTR: Regex =
            Regex::new(r"([a-zA-Z ]+): (\d+)-(\d+) or (\d+)-(\d+)").unwrap();
    }

    let mut lines = input.lines();

    let mut restrictions = Vec::new();
    while let Some(line) = lines.next() {
        if line == "" {
            break;
        }

        let captures = RE_RESTR.captures(line).unwrap();
        restrictions.push(Restrictions {
            name: captures.get(1).unwrap().as_str(),
            first: (
                captures.get(2).unwrap().as_str().parse().unwrap(),
                captures.get(3).unwrap().as_str().parse().unwrap(),
            ),
            second: (
                captures.get(4).unwrap().as_str().parse().unwrap(),
                captures.get(5).unwrap().as_str().parse().unwrap(),
            ),
        });
    }

    // skip "your ticket:" line
    lines.next();
    let own_ticket = lines.next().unwrap().parse().unwrap();

    // skip over empty and "nearby tickets" lines
    lines.next();
    lines.next();

    let other_tickets = lines.map(|l| l.parse().unwrap());

    (restrictions, own_ticket, other_tickets)
}

fn matches_restr(num: u64, restr: &Restrictions) -> bool {
    (num >= restr.first.0 && num <= restr.first.1)
        || (num >= restr.second.0 && num <= restr.second.1)
}

pub fn part1(input: &str) -> u64 {
    let (restrictions, _, tickets) = parse(input);

    tickets
        .flat_map(|t| t.numbers)
        .filter(|n| !restrictions.iter().any(|r| matches_restr(*n, r)))
        .sum()
}

pub fn part2(input: &str) -> u64 {
    let (restrictions, my_ticket, tickets) = parse(input);

    let mut possible = vec![vec![true; my_ticket.numbers.len()]; restrictions.len()];

    let mut to_clear = Vec::with_capacity(my_ticket.numbers.len());
    for ticket in tickets {
        let mut any_invalid = false;

        for i in 0..ticket.numbers.len() {
            let mut matches_any = false;
            for j in 0..restrictions.len() {
                if matches_restr(ticket.numbers[i], &restrictions[j]) {
                    matches_any = true;
                } else {
                    to_clear.push((j, i));
                }
            }

            if !matches_any {
                any_invalid = true;
            }
        }

        if !any_invalid {
            for &(j, i) in &to_clear {
                possible[j][i] = false;
            }
        }

        to_clear.clear();
    }

    println!("possible: {:?}", possible);

    let mut product = 1;
    for j in 0..restrictions.len() {
        if restrictions[j].name.starts_with("departure") {
            let idx = possible[j].iter().position(|p| *p).unwrap();
            println!(
                "Found {} at {}, my number {}",
                restrictions[j].name, idx, my_ticket.numbers[idx]
            );
            product *= my_ticket.numbers[idx];
        }
    }

    product
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn p1_input() {
        let input = std::fs::read_to_string("input/day16").expect("reading input file");
        assert_eq!(part1(&input), 21081);
    }

    #[test]
    fn p1_example1() {
        assert_eq!(part1(EXAMPLE1_INPUT), 71);
    }

    //#[test]
    //fn p2_input() {
    //    let input = std::fs::read_to_string("input/day15").expect("reading input file");
    //    assert_eq!(part2(&input), 11962);
    //}

    //#[test]
    //fn p2_example1() {
    //    assert_eq!(part2(EXAMPLE_INPUTS[0]), 175594);
    //    assert_eq!(
    //        &EXAMPLE_INPUTS.iter().map(|s| part2(*s)).collect::<Vec<_>>(),
    //        &[175594, 2578, 3544142, 261214, 6895259, 18, 362]
    //    );
    //}

    const EXAMPLE1_INPUT: &'static str = "\
        class: 1-3 or 5-7\n\
        row: 6-11 or 33-44\n\
        seat: 13-40 or 45-50\n\
        \n\
        your ticket:\n\
        7,1,14\n\
        \n\
        nearby tickets:\n\
        7,3,47\n\
        40,4,50\n\
        55,2,20\n\
        38,6,12";
}
