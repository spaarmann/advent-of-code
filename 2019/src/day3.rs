#[derive(Clone, Copy, Debug)]
enum LineSeg {
    L(i64),
    R(i64),
    D(i64),
    U(i64),
}

#[derive(Debug, Clone)]
struct Path {
    segs: Vec<LineSeg>,
    min: (i64, i64),
    max: (i64, i64),
}

fn parse_path(l: &str) -> Path {
    let mut segs = Vec::new();
    let mut position = (0, 0);
    let mut max = (0, 0);
    let mut min = (0, 0);
    for s in l.split(",") {
        let mut chars = s.chars();
        let dir = chars
            .next()
            .expect("Every comma is followed by an R, L, U, or D");
        let length: i64 = chars
            .collect::<String>()
            .parse()
            .expect("dir followed by a valid integer");
        segs.push(match dir {
            'L' => {
                position.0 -= length;
                LineSeg::L(length)
            }
            'R' => {
                position.0 += length;
                LineSeg::R(length)
            }
            'D' => {
                position.1 -= length;
                LineSeg::D(length)
            }
            'U' => {
                position.1 += length;
                LineSeg::U(length)
            }
            _ => unreachable!(),
        });

        max.0 = i64::max(max.0, position.0);
        max.1 = i64::max(max.1, position.1);
        min.0 = i64::min(min.0, position.0);
        min.1 = i64::min(min.1, position.1);
    }
    Path { segs, max, min }
}

fn walk_path<F>(p: &Path, mut f: F)
where
    F: FnMut((i64, i64)),
{
    let mut current = (0, 0);
    for seg in &p.segs {
        match seg {
            LineSeg::L(length) => {
                for _ in 0..*length {
                    current.0 -= 1;
                    f(current);
                }
            }
            LineSeg::R(length) => {
                for _ in 0..*length {
                    current.0 += 1;
                    f(current);
                }
            }
            LineSeg::D(length) => {
                for _ in 0..*length {
                    current.1 -= 1;
                    f(current);
                }
            }
            LineSeg::U(length) => {
                for _ in 0..*length {
                    current.1 += 1;
                    f(current);
                }
            }
        }
    }
}

#[allow(dead_code)]
fn print_grid(grid: &[u8], size: (i64, i64), min: (i64, i64), max: (i64, i64)) {
    let to_index = |x, y| ((x - min.0) + (y - min.1) * size.0) as usize;
    print!("+");
    for _ in min.0..max.0 {
        print!("-");
    }
    println!("+");

    for y in (min.1..=max.1).rev() {
        print!("|");
        for x in min.0..=max.0 {
            if x == 0 && y == 0 {
                print!("o");
                continue;
            }
            let val = grid[to_index(x, y)];
            match val {
                0 => print!("."),
                1 => print!("r"),
                2 => print!("j"),
                3 => print!("X"),
                _ => unreachable!(),
            };
        }
        println!("|");
    }

    print!("+");
    for _ in min.0..max.0 {
        print!("-");
    }
    println!("+");
}

pub fn part1(input: &str) -> u64 {
    let mut lines = input.lines();
    let path1 = parse_path(lines.next().expect("there is a path"));
    let path2 = parse_path(lines.next().expect("there are two paths"));

    let min = (
        i64::min(path1.min.0, path2.min.0),
        i64::min(path1.min.1, path2.min.1),
    );
    let max = (
        i64::max(path1.max.0, path2.max.0),
        i64::max(path1.max.1, path2.max.1),
    );
    let size = (max.0 + 1 - min.0, max.1 + 1 - min.1);

    let mut grid = vec![0u8; (size.0 * size.1) as usize];

    let to_index = |x, y| ((x - min.0) + (y - min.1) * size.0) as usize;

    walk_path(&path1, |(x, y)| {
        grid[to_index(x, y)] |= 1;
    });
    walk_path(&path2, |(x, y)| {
        grid[to_index(x, y)] |= 2;
    });

    //print_grid(&grid, size, min, max);

    let mut closest = u64::MAX;
    for y in min.1..=max.1 {
        for x in min.0..=max.0 {
            if x == 0 && y == 0 {
                continue;
            }
            if grid[to_index(x, y)] == 3 {
                let dist = (x.abs() + y.abs()) as u64;
                if dist < closest {
                    closest = dist;
                }
            }
        }
    }

    closest
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn day3_example1() {
        let input = "R75,D30,R83,U83,L12,D49,R71,U7,L72\nU62,R66,U55,R34,D71,R55,D58,R83";
        assert_eq!(part1(input), 159);
    }

    #[test]
    fn day3_example2() {
        let input =
            "R98,U47,R26,D63,R33,U87,L62,D20,R33,U53,R51\nU98,R91,D20,R16,D67,R40,U7,R15,U6,R7";
        assert_eq!(part1(input), 135);
    }
}
