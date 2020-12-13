fn parse(input: &str) -> (u64, impl Iterator<Item = Option<u64>> + '_) {
    let mut lines = input.lines();
    let time = lines.next().unwrap().parse().unwrap();
    let buses = lines.next().unwrap().split(',').map(|s| {
        if s == "x" {
            None
        } else {
            Some(s.parse().unwrap())
        }
    });
    (time, buses)
}

pub fn part1(input: &str) -> u64 {
    let (time, buses) = parse(input);
    let buses = buses.filter_map(|x| x); // discard Nones, i.e. 'x' inputs

    let buses_with_time_to_wait = buses.map(|b| (b, b - time % b));
    let next_bus = buses_with_time_to_wait.min_by_key(|&(_, t)| t).unwrap();

    next_bus.0 * next_bus.1
}

pub fn part2(_input: &str) -> u64 {
    // This requires some interesting math, but all my capacity for learning new math is currently
    // taken up by writing a bachelor thesis, so this part will have to wait for another day.

    0
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn p1_input() {
        let input = std::fs::read_to_string("input/day13").expect("reading input file");
        assert_eq!(part1(&input), 2165);
    }

    #[test]
    fn p1_example1() {
        assert_eq!(part1(EXAMPLE1_INPUT), 295);
    }

    //#[test]
    //fn p2_input() {
    //    let input = std::fs::read_to_string("input/day13").expect("reading input file");
    //    assert_eq!(part2(&input), 138669);
    //}

    //#[test]
    //fn p2_example1() {
    //    assert_eq!(part2(EXAMPLE1_INPUT), 1068781);
    //}

    const EXAMPLE1_INPUT: &'static str = "\
        939\n\
        7,13,x,x,59,x,31,19";
}
