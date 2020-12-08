#[derive(Copy, Clone, Debug, Eq, PartialEq)]
enum Op {
    Nop,
    Acc,
    Jmp,
}
use Op::*;

struct Instr {
    op: Op,
    num: i64,
}

fn parse(input: &str) -> impl Iterator<Item = Instr> + '_ {
    input.lines().map(|line| {
        let op = match &line[..3] {
            "nop" => Nop,
            "acc" => Acc,
            "jmp" => Jmp,
            _ => panic!("invalid instruction!"),
        };
        let num = line[4..].parse().expect("valid offset");
        Instr { op, num }
    })
}

fn run_program(program: &[Instr]) -> (i64, bool) {
    let mut visited = vec![false; program.len()];
    let mut acc = 0;
    let mut index = 0isize;
    let len = program.len() as isize;

    while index < len && !visited[index as usize] {
        visited[index as usize] = true;
        (index, acc) = run_step(&program, (index, acc));
    }

    (acc, index == len)
}

fn run_step(program: &[Instr], state: (isize, i64)) -> (isize, i64) {
    let instr = &program[state.0 as usize];
    match instr.op {
        Nop => (state.0 + 1, state.1),
        Acc => (state.0 + 1, state.1 + instr.num),
        Jmp => (state.0 + instr.num as isize, state.1),
    }
}

pub fn part1(input: &str) -> i64 {
    let program = parse(input).collect::<Vec<_>>();
    run_program(&program).0
}

fn try_replacement(program: &mut [Instr], i: usize, replacement: Op) -> (i64, bool) {
    if program[i].op == replacement {
        return (0, false);
    }

    let orig = program[i].op;
    program[i].op = replacement;
    let (result, terminated) = run_program(program);
    program[i].op = orig;

    (result, terminated)
}

pub fn part2(input: &str) -> i64 {
    let mut program = parse(input).collect::<Vec<_>>();

    let (result, terminated) = run_program(&program);
    if terminated {
        return result;
    };

    for i in 0..program.len() {
        if program[i].op == Acc {
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
