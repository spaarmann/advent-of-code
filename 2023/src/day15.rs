fn hash(s: &str) -> u8 {
    s.bytes()
        .fold(0, |curr, b| curr.wrapping_add(b).wrapping_mul(17))
}

pub fn part1(input: &str) -> u64 {
    input.trim().split(',').map(|step| hash(step) as u64).sum()
}

pub fn part2(input: &str) -> u64 {
    let mut boxes = vec![Vec::new(); 256];

    for step in input.trim().split(',') {
        let op_idx = step.find('=').or_else(|| step.find('-')).unwrap();
        let label = &step[0..op_idx];
        let box_idx = hash(label) as usize;
        let bx = &mut boxes[box_idx];
        let existing = bx.iter().position(|(_, lbl)| *lbl == label);
        match step.as_bytes()[op_idx] {
            b'=' => {
                let focal_length = step.as_bytes()[op_idx + 1] - b'0';
                if let Some(idx) = existing {
                    bx[idx] = (focal_length, label);
                } else {
                    bx.push((focal_length, label));
                }
            }
            b'-' => {
                if let Some(idx) = existing {
                    bx.remove(idx);
                }
            }
            _ => unreachable!(),
        }
    }

    boxes
        .into_iter()
        .enumerate()
        .map(|(i, bx)| {
            bx.into_iter()
                .enumerate()
                .map(|(bi, (fl, _))| (i + 1) * (bi + 1) * fl as usize)
                .sum::<usize>() as u64
        })
        .sum()
}

#[cfg(test)]
mod tests {
    use super::*;

    const EXAMPLE_INPUT: &'static str = "rn=1,cm-,qp=3,cm=2,qp-,pc=4,ot=9,ab=5,pc-,pc=6,ot=7";

    #[test]
    fn p1_example() {
        assert_eq!(part1(EXAMPLE_INPUT), 1320);
    }

    #[test]
    fn p1_input() {
        let input = std::fs::read_to_string("input/day15").expect("reading input file");
        assert_eq!(part1(&input), 514025);
    }

    #[test]
    fn p2_example() {
        assert_eq!(part2(EXAMPLE_INPUT), 145);
    }

    #[test]
    fn p2_input() {
        let input = std::fs::read_to_string("input/day15").expect("reading input file");
        assert_eq!(part2(&input), 244461);
    }
}
