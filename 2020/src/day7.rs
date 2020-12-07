use lazy_static::lazy_static;
use regex::Regex;
use std::collections::HashMap;

type BagMap<'a> = HashMap<&'a str, Vec<(u64, &'a str)>>;

fn parse(input: &str) -> BagMap {
    let mut bags = HashMap::new();

    lazy_static! {
        static ref CONTAINER_RE: Regex = Regex::new(r"^(\w+ \w+) bags contain ").unwrap();
        static ref CARRIED_RE: Regex = Regex::new(r"(\d+) (\w+ \w+) bag").unwrap();
    }

    for line in input.lines() {
        let container_match = &CONTAINER_RE
            .captures(line)
            .and_then(|c| c.get(1))
            .expect("format");
        let container = container_match.as_str();
        let carried_bags_text = &line[container_match.end()..];

        if carried_bags_text == "no other bags." {
            bags.insert(container, Vec::new());
            continue;
        }

        let mut carried_bags = Vec::new();
        for captures in CARRIED_RE.captures_iter(carried_bags_text) {
            let count = captures
                .get(1)
                .and_then(|c| c.as_str().parse().ok())
                .expect("format");
            let bag = captures.get(2).expect("format");
            carried_bags.push((count, bag.as_str()));
        }

        bags.insert(container, carried_bags);
    }

    bags
}

const GOAL: &'static str = "shiny gold";

pub fn part1(input: &str) -> u64 {
    let mut bags = parse(input);
    let mut containing_goal = Vec::new();

    // Find the ones directly containing our goal.
    for (container, carried) in bags.clone() {
        if carried.iter().any(|(_, c)| c == &GOAL) {
            containing_goal.push(container);
            bags.remove(container);
        }
    }

    // Now iteratively find everything that contains
    // any bag that we know can contain a goal one.
    let mut found_one = true;
    while found_one {
        found_one = false;

        'outer: for (container, carried) in bags.clone() {
            for b in carried {
                if containing_goal.contains(&b.1) {
                    containing_goal.push(container);
                    bags.remove(container);
                    found_one = true;
                    continue 'outer;
                }
            }
        }
    }

    containing_goal.len() as u64
}

fn count_contained_in(bags: &BagMap, b: &str) -> u64 {
    let contained = bags.get(b).unwrap();
    let total = contained.iter().fold(0, |acc, (count, c)| {
        acc + count * (1 + count_contained_in(&bags, c))
    });
    total
}

pub fn part2(input: &str) -> u64 {
    let bags = parse(input);
    count_contained_in(&bags, GOAL)
}

#[cfg(test)]
mod tests {
    use super::*;

    const EXAMPLE1_INPUT: &'static str = "\
        light red bags contain 1 bright white bag, 2 muted yellow bags.\n\
        dark orange bags contain 3 bright white bags, 4 muted yellow bags.\n\
        bright white bags contain 1 shiny gold bag.\n\
        muted yellow bags contain 2 shiny gold bags, 9 faded blue bags.\n\
        shiny gold bags contain 1 dark olive bag, 2 vibrant plum bags.\n\
        dark olive bags contain 3 faded blue bags, 4 dotted black bags.\n\
        vibrant plum bags contain 5 faded blue bags, 6 dotted black bags.\n\
        faded blue bags contain no other bags.\n\
        dotted black bags contain no other bags.";

    #[test]
    fn p1_input() {
        let input = std::fs::read_to_string("input/day7").expect("reading input file");
        assert_eq!(part1(&input), 238);
    }

    #[test]
    fn p1_example1() {
        assert_eq!(part1(EXAMPLE1_INPUT), 4);
    }

    #[test]
    fn p2_input() {
        let input = std::fs::read_to_string("input/day7").expect("reading input file");
        assert_eq!(part2(&input), 82930);
    }

    #[test]
    fn p2_example1() {
        assert_eq!(part2(EXAMPLE1_INPUT), 32);
    }
}
