#[derive(Clone, Debug)]
struct Counter {
    red: u64,
    green: u64,
    blue: u64,
}

impl Counter {
    fn new() -> Self {
        Self {
            red: 0,
            green: 0,
            blue: 0,
        }
    }

    fn limits() -> Self {
        Self {
            red: 12,
            green: 13,
            blue: 14,
        }
    }

    fn leq(&self, other: &Self) -> bool {
        self.red <= other.red && self.green <= other.green && self.blue <= other.blue
    }

    fn max(self, other: Self) -> Self {
        Self {
            red: self.red.max(other.red),
            green: self.green.max(other.green),
            blue: self.blue.max(other.blue),
        }
    }

    fn set(&mut self, colour: &str, num: u64) {
        match colour {
            "red" => self.red = num,
            "green" => self.green = num,
            "blue" => self.blue = num,
            _ => panic!("unknown colour"),
        }
    }
}

fn parse(input: &str) -> impl Iterator<Item = impl Iterator<Item = Counter> + '_> {
    input.lines().map(|l| {
        l[(l.find(':').unwrap() + 2)..].split("; ").map(|game| {
            let mut counter = Counter::new();
            for draw in game.split(", ") {
                let (num, colour) = draw.split_once(' ').unwrap();
                let num = num.parse::<u64>().unwrap();
                counter.set(colour, num);
            }
            counter
        })
    })
}

pub fn part1(input: &str) -> u64 {
    let limits = Counter::limits();
    parse(input)
        .enumerate()
        .filter_map(|(i, mut game)| {
            if game.all(|c| c.leq(&limits)) {
                Some(i + 1)
            } else {
                None
            }
        })
        .sum::<usize>() as u64
}

pub fn part2(input: &str) -> u64 {
    parse(input)
        .map(|game| game.fold(Counter::new(), Counter::max))
        .map(|c| c.red * c.green * c.blue)
        .sum()
}

#[cfg(test)]
mod tests {
    use super::*;

    const EXAMPLE_INPUT: &'static str = "Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green
Game 2: 1 blue, 2 green; 3 green, 4 blue, 1 red; 1 green, 1 blue
Game 3: 8 green, 6 blue, 20 red; 5 blue, 4 red, 13 green; 5 green, 1 red
Game 4: 1 green, 3 red, 6 blue; 3 green, 6 red; 3 green, 15 blue, 14 red
Game 5: 6 red, 1 blue, 3 green; 2 blue, 1 red, 2 green
";

    #[test]
    fn p1_example() {
        assert_eq!(part1(EXAMPLE_INPUT), 8);
    }

    #[test]
    fn p1_input() {
        let input = std::fs::read_to_string("input/day2").expect("reading input file");
        assert_eq!(part1(&input), 2810);
    }

    #[test]
    fn p2_example() {
        assert_eq!(part2(EXAMPLE_INPUT), 2286);
    }

    #[test]
    fn p2_input() {
        let input = std::fs::read_to_string("input/day2").expect("reading input file");
        assert_eq!(part2(&input), 69110);
    }
}
