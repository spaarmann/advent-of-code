#[derive(Copy, Clone, Debug, PartialEq, Eq)]
enum Spot {
    Empty,
    Occupied,
    Floor,
}

use Spot::*;

impl From<char> for Spot {
    fn from(c: char) -> Self {
        match c {
            'L' => Empty,
            '#' => Occupied,
            '.' => Floor,
            _ => panic!("invalid character!"),
        }
    }
}

fn parse(input: &str) -> Vec<Vec<Spot>> {
    input
        .lines()
        .map(|line| line.chars().map(|c| c.into()).collect::<Vec<_>>())
        .collect::<Vec<_>>()
}

#[allow(unused)]
fn print_grid(grid: &Vec<Vec<Spot>>) {
    for y in 0..grid.len() {
        for x in 0..grid[0].len() {
            print!(
                "{}",
                match grid[y][x] {
                    Empty => 'L',
                    Occupied => '#',
                    Floor => '.',
                }
            );
        }
        println!();
    }
}

const DIRS: [(i64, i64); 8] = [
    (1, 0),
    (-1, 0),
    (0, 1),
    (0, -1),
    (1, 1),
    (-1, 1),
    (1, -1),
    (-1, -1),
];

fn count_adjacent(grid: &Vec<Vec<Spot>>, x: usize, y: usize) -> u8 {
    let (width, height) = (grid[0].len() as i64, grid.len() as i64);
    let mut count = 0;
    for (x_dir, y_dir) in &DIRS {
        let (mut x, mut y) = (x as i64, y as i64);
        x += x_dir;
        y += y_dir;
        if x >= 0 && x < width && y >= 0 && y < height {
            match grid[y as usize][x as usize] {
                Occupied => count += 1,
                Empty => {}
                Floor => {}
            }
        }
    }

    count
}

fn count_raytraced(grid: &Vec<Vec<Spot>>, x: usize, y: usize) -> u8 {
    let (width, height) = (grid[0].len() as i64, grid.len() as i64);
    let mut count = 0;
    'outer: for (x_dir, y_dir) in &DIRS {
        let (mut x, mut y) = (x as i64, y as i64);
        x += x_dir;
        y += y_dir;
        while x >= 0 && x < width && y >= 0 && y < height {
            match unsafe { grid.get_unchecked(y as usize).get_unchecked(x as usize) } {
                Occupied => {
                    count += 1;
                    continue 'outer;
                }
                Empty => {
                    continue 'outer;
                }
                Floor => {}
            }
            x += x_dir;
            y += y_dir;
        }
    }

    count
}

fn run_simulation<F>(input: &str, rule: F) -> (u64, u64)
where
    F: Fn(&Vec<Vec<Spot>>, usize, usize) -> (Spot, bool),
{
    let mut grid = parse(input);
    let mut next = grid.clone();

    let (width, height) = (grid[0].len(), grid.len());

    let mut has_changed = true;
    let mut iterations = 0;
    while has_changed {
        has_changed = false;

        for y in 0..height {
            for x in 0..width {
                let (new, changed) = rule(&grid, x, y);
                next[y][x] = new;
                has_changed |= changed;
            }
        }

        (grid, next) = (next, grid);
        iterations += 1;
    }

    (
        grid.iter()
            .map(|row| row.iter().filter(|&&s| s == Occupied).count())
            .sum::<usize>() as _,
        iterations,
    )
}

pub fn part1(input: &str) -> u64 {
    let update_rule = |grid: &Vec<Vec<Spot>>, x: usize, y: usize| match grid[y][x] {
        Empty if count_adjacent(grid, x, y) == 0 => (Occupied, true),
        Occupied if count_adjacent(grid, x, y) >= 4 => (Empty, true),
        prev => (prev, false),
    };

    let (result, iterations) = run_simulation(input, update_rule);
    println!("Finished day11-1 after {} iterations.", iterations);
    result
}

pub fn part2(input: &str) -> u64 {
    let update_rule = |grid: &Vec<Vec<Spot>>, x: usize, y: usize| match grid[y][x] {
        Empty if count_raytraced(grid, x, y) == 0 => (Occupied, true),
        Occupied if count_raytraced(grid, x, y) >= 5 => (Empty, true),
        prev => (prev, false),
    };

    let (result, iterations) = run_simulation(input, update_rule);
    println!("Finished day11-2 after {} iterations.", iterations);
    result
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn p1_input() {
        let input = std::fs::read_to_string("input/day11").expect("reading input file");
        assert_eq!(part1(&input), 2126);
    }

    #[test]
    fn p1_example1() {
        assert_eq!(part1(EXAMPLE1_INPUT), 37);
    }

    #[test]
    fn p2_input() {
        let input = std::fs::read_to_string("input/day11").expect("reading input file");
        assert_eq!(part2(&input), 1914);
    }

    #[test]
    fn p2_example1() {
        assert_eq!(part2(EXAMPLE1_INPUT), 26);
    }

    const EXAMPLE1_INPUT: &'static str = "\
        L.LL.LL.LL\n\
        LLLLLLL.LL\n\
        L.L.L..L..\n\
        LLLL.LL.LL\n\
        L.LL.LL.LL\n\
        L.LLLLL.LL\n\
        ..L.L.....\n\
        LLLLLLLLLL\n\
        L.LLLLLL.L\n\
        L.LLLLL.LL";
}
