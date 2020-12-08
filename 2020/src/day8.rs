#[derive(Copy, Clone, Debug, Eq, PartialEq)]
enum Instr {
    Nop,
    Acc,
    Jmp,
}

use Instr::*;

fn parse(input: &str) -> impl Iterator<Item = (Instr, isize)> + '_ {
    input.lines().map(|line| {
        let instr = match &line[..3] {
            "nop" => Nop,
            "acc" => Acc,
            "jmp" => Jmp,
            _ => panic!("invalid instruction!"),
        };
        let offset = line[4..].parse().expect("valid offset");
        (instr, offset)
    })
}

fn evaluate_program(program: &[(Instr, isize)]) -> (i64, bool) {
    let mut visited = vec![false; program.len()];
    let mut acc = 0;
    let mut index = 0isize;
    let len = program.len() as isize;

    while index < len && !visited[index as usize] {
        let (instr, offset) = program[index as usize];
        visited[index as usize] = true;
        match instr {
            Acc => {
                acc += offset as i64;
                index += 1
            }
            Jmp => index += offset,
            Nop => index += 1,
        };
    }

    (acc, index == len)
}

pub fn part1(input: &str) -> i64 {
    let program = parse(input).collect::<Vec<_>>();
    evaluate_program(&program).0
}

fn try_replacement(program: &mut [(Instr, isize)], i: usize, replacement: Instr) -> (i64, bool) {
    if program[i].0 == replacement {
        return (0, false);
    }

    let orig = program[i].0;
    program[i].0 = replacement;
    let (result, terminated) = evaluate_program(program);
    program[i].0 = orig;

    (result, terminated)
}

pub fn part2(input: &str) -> i64 {
    let mut program = parse(input).collect::<Vec<_>>();

    for i in 0..program.len() {
        let (result, terminated) = evaluate_program(&program);
        if terminated {
            return result;
        };

        if program[i].0 == Acc {
            continue;
        }

        let (result, terminated) = try_replacement(&mut program, i, Jmp);
        if terminated {
            return result;
        };
        let (result, terminated) = try_replacement(&mut program, i, Nop);
        if terminated {
            return result;
        };
    }

    panic!("program never terminated!");
}

#[cfg(test)]
mod tests {
    use super::*;

    const EXAMPLE1_INPUT: &'static str = "\
        nop +0\n\
        acc +1\n\
        jmp +4\n\
        acc +3\n\
        jmp -3\n\
        acc -99\n\
        acc +1\n\
        jmp -4\n\
        acc +6";

    #[test]
    fn p1_input() {
        let input = std::fs::read_to_string("input/day8").expect("reading input file");
        assert_eq!(part1(&input), 1137);
    }

    #[test]
    fn p1_example1() {
        assert_eq!(part1(EXAMPLE1_INPUT), 5);
    }

    #[test]
    fn p2_input() {
        let input = std::fs::read_to_string("input/day8").expect("reading input file");
        assert_eq!(part2(&input), 1125);
    }

    #[test]
    fn p2_example1() {
        assert_eq!(part2(EXAMPLE1_INPUT), 8);
    }
}
