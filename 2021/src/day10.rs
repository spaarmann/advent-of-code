fn closing_delim(c: char) -> char {
    match c {
        '(' => ')',
        '[' => ']',
        '{' => '}',
        '<' => '>',
        _ => panic!("unknown delim char: {}", c),
    }
}

fn score_for_corrupt_closing_delim(c: char) -> u64 {
    match c {
        ')' => 3,
        ']' => 57,
        '}' => 1197,
        '>' => 25137,
        _ => 0,
    }
}

pub fn part1(input: &str) -> u64 {
    let mut score = 0;

    for line in input.lines() {
        let mut stack = Vec::new();
        for c in line.chars() {
            match c {
                '(' | '[' | '{' | '<' => stack.push(c),
                ')' | ']' | '}' | '>' => {
                    let top = stack.pop().expect("stack should not get emptied");
                    if closing_delim(top) != c {
                        score += score_for_corrupt_closing_delim(c);
                        break;
                    }
                }
                _ => (),
            }
        }
    }

    score
}

fn score_for_completed_closing_delim(c: char) -> u64 {
    match c {
        ')' => 1,
        ']' => 2,
        '}' => 3,
        '>' => 4,
        _ => 0,
    }
}

pub fn part2(input: &str) -> u64 {
    let mut scores = Vec::new();

    'lines: for line in input.lines() {
        let mut stack = Vec::new();
        for c in line.chars() {
            match c {
                '(' | '[' | '{' | '<' => stack.push(c),
                ')' | ']' | '}' | '>' => {
                    let top = stack.pop().expect("stack should not get emptied");
                    if closing_delim(top) != c {
                        // Just ignore corrupted lines
                        continue 'lines;
                    }
                }
                _ => (),
            }
        }

        let mut score = 0;
        for unclosed in stack.drain(..).rev() {
            let closing = closing_delim(unclosed);
            score = score * 5 + score_for_completed_closing_delim(closing);
        }

        scores.push(score);
    }

    scores.sort();

    scores[scores.len() / 2]
}

#[cfg(test)]
mod tests {
    use super::*;

    const EXAMPLE_INPUT: &'static str = "[({(<(())[]>[[{[]{<()<>>
[(()[<>])]({[<{<<[]>>(
{([(<{}[<>[]}>{[]{[(<()>
(((({<>}<{<{<>}{[]{[]{}
[[<[([]))<([[{}[[()]]]
[{[{({}]{}}([{[{{{}}([]
{<[[]]>}<{[{[{[]{()[[[]
[<(<(<(<{}))><([]([]()
<{([([[(<>()){}]>(<<{{
<{([{{}}[<[[[<>{}]]]>[]]
";

    #[test]
    fn p1_example() {
        assert_eq!(part1(EXAMPLE_INPUT), 26397);
    }

    #[test]
    fn p1_input() {
        let input = std::fs::read_to_string("input/day10").expect("reading input file");
        assert_eq!(part1(&input), 318081);
    }

    #[test]
    fn p2_example() {
        assert_eq!(part2(EXAMPLE_INPUT), 288957);
    }

    #[test]
    fn p2_input() {
        let input = std::fs::read_to_string("input/day10").expect("reading input file");
        assert_eq!(part2(&input), 4361305341);
    }
}
