use std::collections::HashMap;

struct CaveSystem {
    caves: Vec<(String, bool)>,
    connections: Vec<Vec<usize>>,
    start: usize,
    end: usize,
}

impl CaveSystem {
    fn parse(input: &str) -> CaveSystem {
        let mut caves = Vec::new();
        let mut connections = Vec::new();
        let mut name_to_idx = HashMap::new();

        let mut start = 0;
        let mut end = 0;

        for line in input.lines() {
            let (first, second) = line.split_once('-').unwrap();

            let first_idx = *name_to_idx.entry(first).or_insert_with(|| {
                caves.push((
                    first.to_string(),
                    first.chars().next().unwrap().is_uppercase(),
                ));
                connections.push(Vec::new());
                caves.len() - 1
            });
            let second_idx = *name_to_idx.entry(second).or_insert_with(|| {
                caves.push((
                    second.to_string(),
                    second.chars().next().unwrap().is_uppercase(),
                ));
                connections.push(Vec::new());
                caves.len() - 1
            });

            connections[first_idx].push(second_idx);
            connections[second_idx].push(first_idx);

            if first == "start" {
                start = first_idx;
            }
            if first == "end" {
                end = first_idx;
            }
            if second == "start" {
                start = second_idx;
            }
            if second == "end" {
                end = second_idx;
            }
        }

        CaveSystem {
            caves,
            connections,
            start,
            end,
        }
    }

    fn count_paths(&self, can_use_double_small_cave: bool) -> usize {
        let mut start = vec![self.start];

        let mut flags = vec![false; self.caves.len()];
        flags[self.start] = true;

        self.count_paths_rec(&mut start, &mut flags, can_use_double_small_cave)
    }

    fn count_paths_rec(
        &self,
        prefix: &mut Vec<usize>,
        used_flags: &mut [bool],
        can_still_use_double_small_cave: bool,
    ) -> usize {
        let current_end = *prefix.last().unwrap();
        if current_end == self.end {
            return 1;
        }

        let mut count = 0;

        let end_connections = &self.connections[current_end];
        for &connection in end_connections {
            // Can walk down this path if it is a big cave, so we can visit it twice, or if it
            // small but we haven't visited it before on this path, ...
            let mut can_use_connection = self.caves[connection].1 || !used_flags[connection];

            // ... or if it is small, but not start or end, and we have visited it before, but we
            // haven't visited any other small cave twice.
            let mut new_can_use_double = can_still_use_double_small_cave;
            let mut used_double = false;
            if !can_use_connection
                && can_still_use_double_small_cave
                && connection != self.start
                && connection != self.end
            {
                can_use_connection = true;
                new_can_use_double = false;
                used_double = true;
            }

            if !can_use_connection {
                continue;
            }

            prefix.push(connection);
            used_flags[connection] = true;

            count += self.count_paths_rec(prefix, used_flags, new_can_use_double);

            prefix.pop();
            if !used_double {
                used_flags[connection] = false;
            }
        }

        count
    }
}

pub fn part1(input: &str) -> u64 {
    let system = CaveSystem::parse(input);
    system.count_paths(false) as u64
}

pub fn part2(input: &str) -> u64 {
    let system = CaveSystem::parse(input);
    system.count_paths(true) as u64
}

#[cfg(test)]
mod tests {
    use super::*;

    const EXAMPLE1_INPUT: &'static str = "start-A
start-b
A-c
A-b
b-d
A-end
b-end
";

    const EXAMPLE2_INPUT: &'static str = "dc-end
HN-start
start-kj
dc-start
dc-HN
LN-dc
HN-end
kj-sa
kj-HN
kj-dc";

    const EXAMPLE3_INPUT: &'static str = "fs-end
he-DX
fs-he
start-DX
pj-DX
end-zg
zg-sl
zg-pj
pj-he
RW-he
fs-DX
pj-RW
zg-RW
start-pj
he-WI
zg-he
pj-fs
start-RW
";

    #[test]
    fn p1_example1() {
        assert_eq!(part1(EXAMPLE1_INPUT), 10);
    }

    #[test]
    fn p1_example2() {
        assert_eq!(part1(EXAMPLE2_INPUT), 19);
    }

    #[test]
    fn p1_example3() {
        assert_eq!(part1(EXAMPLE3_INPUT), 226);
    }

    #[test]
    fn p1_input() {
        let input = std::fs::read_to_string("input/day12").expect("reading input file");
        assert_eq!(part1(&input), 3450);
    }

    #[test]
    fn p2_example1() {
        assert_eq!(part2(EXAMPLE1_INPUT), 36);
    }

    #[test]
    fn p2_example2() {
        assert_eq!(part2(EXAMPLE2_INPUT), 103);
    }

    #[test]
    fn p2_example3() {
        assert_eq!(part2(EXAMPLE3_INPUT), 3509);
    }

    #[test]
    fn p2_input() {
        let input = std::fs::read_to_string("input/day12").expect("reading input file");
        assert_eq!(part2(&input), 96528);
    }
}
