#[derive(Copy, Clone, Debug, PartialEq, Eq)]
enum RPS {
    Rock,
    Paper,
    Scissors,
}

impl RPS {
    fn from_str(s: &str) -> Self {
        match s {
            "A" | "X" => RPS::Rock,
            "B" | "Y" => RPS::Paper,
            "C" | "Z" => RPS::Scissors,
            _ => panic!("invalid RPS string"),
        }
    }

    fn play_against(self, other: Self) -> Outcome {
        use RPS::*;

        match (self, other) {
            (Rock, Scissors) | (Paper, Rock) | (Scissors, Paper) => Outcome::Win,
            (Scissors, Rock) | (Rock, Paper) | (Paper, Scissors) => Outcome::Loss,
            _ => Outcome::Draw,
        }
    }

    fn points_against(self, other: Self) -> u64 {
        use RPS::*;
        let my_score = match self {
            Rock => 1,
            Paper => 2,
            Scissors => 3,
        };
        let outcome_score = self.play_against(other).score();

        return my_score + outcome_score;
    }
}

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
enum Outcome {
    Win,
    Loss,
    Draw,
}

impl Outcome {
    fn from_str(s: &str) -> Self {
        match s {
            "X" => Outcome::Loss,
            "Y" => Outcome::Draw,
            "Z" => Outcome::Win,
            _ => panic!("invalid outcome string"),
        }
    }

    fn score(self) -> u64 {
        match self {
            Outcome::Loss => 0,
            Outcome::Draw => 3,
            Outcome::Win => 6,
        }
    }

    fn required_play(self, first: RPS) -> RPS {
        use Outcome::*;
        use RPS::*;

        match (self, first) {
            (Win, Rock) => Paper,
            (Win, Paper) => Scissors,
            (Win, Scissors) => Rock,
            (Loss, Rock) => Scissors,
            (Loss, Paper) => Rock,
            (Loss, Scissors) => Paper,
            (Draw, Rock) => Rock,
            (Draw, Paper) => Paper,
            (Draw, Scissors) => Scissors,
        }
    }
}

pub fn part1(input: &str) -> u64 {
    input
        .lines()
        .map(|l| {
            let (left, right) = l.split_once(' ').unwrap();
            (RPS::from_str(left), RPS::from_str(right))
        })
        .map(|(other, me)| me.points_against(other))
        .sum()
}

pub fn part2(input: &str) -> u64 {
    input
        .lines()
        .map(|l| {
            let (other, outcome) = l.split_once(' ').unwrap();
            (RPS::from_str(other), Outcome::from_str(outcome))
        })
        .map(|(other, outcome)| outcome.required_play(other).points_against(other))
        .sum()
}

#[cfg(test)]
mod tests {
    use super::*;

    const EXAMPLE_INPUT: &'static str = "A Y
B X
C Z";

    #[test]
    fn p1_example() {
        assert_eq!(part1(EXAMPLE_INPUT), 15);
    }

    #[test]
    fn p1_input() {
        let input = std::fs::read_to_string("input/day2").expect("reading input file");
        assert_eq!(part1(&input), 14375);
    }

    #[test]
    fn p2_example() {
        assert_eq!(part2(EXAMPLE_INPUT), 12);
    }

    #[test]
    fn p2_input() {
        let input = std::fs::read_to_string("input/day2").expect("reading input file");
        assert_eq!(part2(&input), 10274);
    }
}
