use std::collections::HashMap;

#[derive(Debug)]
struct Entry {
    #[allow(dead_code)]
    name: String,
    parent: usize,
    child_dirs: HashMap<String, usize>,
    files: Vec<(String, usize)>,
}

fn parse(input: &str) -> Vec<Entry> {
    // Assume we always start with `cd /`.
    let mut dirs = Vec::new();
    dirs.push(Entry {
        name: "/".to_string(),
        parent: 0,
        child_dirs: HashMap::new(),
        files: Vec::new(),
    });

    let mut lines = input.lines().peekable();
    lines.next(); // skip `cd /`

    let mut pwd = 0;

    while let Some(line) = lines.next() {
        if line == "$ ls" {
            while lines.peek().map(|l| !l.starts_with('$')).unwrap_or(false) {
                let ls_line = lines.next().unwrap();
                let (left, name) = ls_line.split_once(' ').unwrap();
                if left == "dir" {
                    dirs.push(Entry {
                        name: name.to_string(),
                        parent: pwd,
                        child_dirs: HashMap::new(),
                        files: Vec::new(),
                    });
                    let new_idx = dirs.len() - 1;
                    dirs[pwd].child_dirs.insert(name.to_string(), new_idx);
                } else {
                    let size = left.parse().unwrap();
                    dirs[pwd].files.push((name.to_string(), size));
                }
            }
        } else if line.starts_with("$ cd") {
            let target_name = line.split(' ').nth(2).unwrap();
            if target_name == ".." {
                pwd = dirs[pwd].parent;
            } else {
                pwd = dirs[pwd].child_dirs[target_name];
            }
        } else {
            panic!("unknown line: {}", line);
        }
    }

    dirs
}

fn get_total_size(idx: usize, entries: &[Entry], sizes: &mut [Option<usize>]) {
    if sizes[idx].is_some() {
        return;
    }

    let entry = &entries[idx];
    let mut total_size: usize = entry.files.iter().map(|(_, s)| s).sum();
    for (_, &child_idx) in &entry.child_dirs {
        get_total_size(child_idx, entries, sizes);
        total_size += sizes[child_idx].unwrap();
    }

    sizes[idx] = Some(total_size);
}

pub fn part1(input: &str) -> u64 {
    let entries = parse(input);
    let mut sizes = vec![None; entries.len()];
    get_total_size(0, &entries, &mut sizes);

    sizes
        .into_iter()
        .filter_map(|s| s)
        .filter(|&s| s < 100000)
        .sum::<usize>() as u64
}

pub fn part2(input: &str) -> u64 {
    let entries = parse(input);
    let mut sizes = vec![None; entries.len()];
    get_total_size(0, &entries, &mut sizes);

    let missing_capacity = 30000000 - (70000000 - sizes[0].unwrap());
    sizes
        .into_iter()
        .filter_map(|s| s)
        .filter(|&s| s > missing_capacity)
        .min()
        .unwrap() as u64
}

#[cfg(test)]
mod tests {
    use super::*;

    const EXAMPLE_INPUT: &'static str = "$ cd /
$ ls
dir a
14848514 b.txt
8504156 c.dat
dir d
$ cd a
$ ls
dir e
29116 f
2557 g
62596 h.lst
$ cd e
$ ls
584 i
$ cd ..
$ cd ..
$ cd d
$ ls
4060174 j
8033020 d.log
5626152 d.ext
7214296 k";

    #[test]
    fn p1_example() {
        assert_eq!(part1(EXAMPLE_INPUT), 95437);
    }

    #[test]
    fn p1_input() {
        let input = std::fs::read_to_string("input/day7").expect("reading input file");
        assert_eq!(part1(&input), 1118405);
    }

    #[test]
    fn p2_example() {
        assert_eq!(part2(EXAMPLE_INPUT), 24933642);
    }

    #[test]
    fn p2_input() {
        let input = std::fs::read_to_string("input/day7").expect("reading input file");
        assert_eq!(part2(&input), 12545514);
    }
}
