use std::assert_matches::assert_matches;

use itertools::Itertools;

#[derive(Copy, Clone, Debug)]
enum Token {
    OpenPair,
    ClosePair,
    Number(i64),
}

impl Token {
    fn is_number(&self) -> bool {
        if let Token::Number(_) = self {
            true
        } else {
            false
        }
    }

    fn as_number(&self) -> Option<i64> {
        if let Token::Number(n) = self {
            Some(*n)
        } else {
            None
        }
    }

    fn as_mut_number_ref(&mut self) -> Option<&mut i64> {
        if let Token::Number(n) = self {
            Some(n)
        } else {
            None
        }
    }
}

fn parse(input: &str) -> Vec<Vec<Token>> {
    input
        .lines()
        .map(|l| {
            let mut tokens = Vec::new();

            for c in l.chars() {
                match c {
                    '[' => tokens.push(Token::OpenPair),
                    ']' => tokens.push(Token::ClosePair),
                    c if c.is_digit(10) => {
                        tokens.push(Token::Number(c.to_digit(10).unwrap() as i64))
                    }
                    _ => (),
                }
            }

            tokens
        })
        .collect()
}

fn add(a: &mut Vec<Token>, b: Vec<Token>) {
    a.insert(0, Token::OpenPair);
    a.extend(b.into_iter());
    a.push(Token::ClosePair);
}

fn reduce(n: &mut Vec<Token>) {
    loop {
        // Search for a pair nested inside 4 other pairs that needs to be exploded.
        let mut depth = 0;
        let mut explode_idx = None;
        for (i, t) in n.iter().enumerate() {
            match t {
                Token::OpenPair => {
                    depth += 1;

                    if depth == 5 {
                        // 't' is the pair-opener for a pair that must be exploded.
                        explode_idx = Some(i);
                        break;
                    }
                }
                Token::ClosePair => {
                    depth -= 1;
                }
                _ => {}
            }
        }

        if let Some(explode_idx) = explode_idx {
            let Token::Number(left) = n[explode_idx + 1]
                else { panic!("Exploding pair that does not consist of two numbers!") };
            let Token::Number(right) = n[explode_idx + 2]
                else { panic!("Exploding pair that does not consist of two numbers!") };

            if let Some(t) = n[..explode_idx].iter_mut().rev().find(|t| t.is_number()) {
                *t.as_mut_number_ref().unwrap() += left;
            }

            if let Some(t) = n[(explode_idx + 3)..].iter_mut().find(|t| t.is_number()) {
                *t.as_mut_number_ref().unwrap() += right;
            }

            n.splice(explode_idx..(explode_idx + 4), [Token::Number(0)]);

            // Changed something, try reducing from the start again.
            continue;
        }

        // If there is no further exploding to be done, check if we need to split anything.
        let split_idx = n.iter().find_position(|t| t.as_number().unwrap_or(0) >= 10);
        if let Some((split_idx, _)) = split_idx {
            let value = n[split_idx].as_number().unwrap();
            let left = value / 2;
            let right = (value + 1) / 2;
            n.splice(
                split_idx..=split_idx,
                [
                    Token::OpenPair,
                    Token::Number(left),
                    Token::Number(right),
                    Token::ClosePair,
                ],
            );

            continue;
        }

        // Didn't explode or split anything, so the number is now reduced.
        break;
    }
}

fn rec_magnitude(n: &[Token]) -> (usize, i64) {
    match n {
        [] => (0, 0),
        [Token::Number(n), ..] => (1, *n),
        [Token::OpenPair, ..] => {
            let (left_read, left_mag) = rec_magnitude(&n[1..]);
            let (right_read, right_mag) = rec_magnitude(&n[(left_read + 1)..]);
            assert_matches!(n[1 + left_read + right_read], Token::ClosePair);
            (2 + left_read + right_read, left_mag * 3 + right_mag * 2)
        }
        _ => panic!("Unexpected rec_magnitude input!"),
    }
}

fn magnitude(n: &[Token]) -> i64 {
    rec_magnitude(n).1
}

pub fn part1(input: &str) -> u64 {
    let numbers = parse(input);
    let sum = numbers
        .into_iter()
        .fold1(|mut a, b| {
            add(&mut a, b);
            reduce(&mut a);
            a
        })
        .unwrap();

    magnitude(&sum) as u64
}

pub fn part2(input: &str) -> u64 {
    let numbers = parse(input);

    numbers
        .into_iter()
        .permutations(2)
        .map(|ns| {
            let mut it = ns.into_iter();
            let mut a = it.next().unwrap();
            let b = it.next().unwrap();
            add(&mut a, b);
            reduce(&mut a);
            magnitude(&a) as u64
        })
        .max()
        .unwrap()
}

#[cfg(test)]
mod tests {
    use super::*;

    const EXAMPLE_INPUT: &'static str = "[[[0,[5,8]],[[1,7],[9,6]]],[[4,[1,2]],[[1,4],2]]]
[[[5,[2,8]],4],[5,[[9,9],0]]]
[6,[[[6,2],[5,6]],[[7,6],[4,7]]]]
[[[6,[0,7]],[0,9]],[4,[9,[9,0]]]]
[[[7,[6,4]],[3,[1,3]]],[[[5,5],1],9]]
[[6,[[7,3],[3,2]]],[[[3,8],[5,7]],4]]
[[[[5,4],[7,7]],8],[[8,3],8]]
[[9,3],[[9,9],[6,[4,9]]]]
[[2,[[7,7],7]],[[5,8],[[9,3],[0,2]]]]
[[[[5,2],5],[8,[3,7]]],[[5,[7,5]],[4,4]]]
";

    #[test]
    fn p1_example() {
        assert_eq!(part1(EXAMPLE_INPUT), 4140);
    }

    #[test]
    fn p1_input() {
        let input = std::fs::read_to_string("input/day18").expect("reading input file");
        assert_eq!(part1(&input), 4176);
    }

    #[test]
    fn p2_example() {
        assert_eq!(part2(EXAMPLE_INPUT), 3993);
    }

    #[test]
    fn p2_input() {
        let input = std::fs::read_to_string("input/day18").expect("reading input file");
        assert_eq!(part2(&input), 4633);
    }
}
