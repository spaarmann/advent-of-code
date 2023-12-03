use std::ops::{Index, IndexMut, Range};

use itertools::Itertools;

#[derive(Clone)]
struct Schematic {
    schematic: Vec<Vec<char>>,
    w: usize,
    h: usize,
}
impl Schematic {
    fn parse(input: &str) -> Self {
        let schematic = input.lines().map(|l| l.chars().collect_vec()).collect_vec();
        let w = schematic[0].len();
        let h = schematic.len();
        Self { schematic, w, h }
    }

    fn in_bounds(&self, x: i64, y: i64) -> bool {
        x >= 0 && (x as usize) < self.w && y >= 0 && (y as usize) < self.h
    }

    fn find_and_clear_adjacent_numbers(&mut self, nums: &mut Vec<i64>, x: i64, y: i64) {
        let neighbors: [(i64, i64); 8] = [
            (-1, -1),
            (-1, 0),
            (-1, 1),
            (0, -1),
            (0, 1),
            (1, -1),
            (1, 0),
            (1, 1),
        ];

        let find_number = |schematic: &mut Schematic, mut x: i64, y: i64| {
            if !schematic[(x, y)].is_ascii_digit() {
                return None;
            }

            while schematic.in_bounds(x - 1, y) && schematic[(x - 1, y)].is_ascii_digit() {
                x -= 1;
            }
            let start_x = x;
            while schematic.in_bounds(x + 1, y) && schematic[(x + 1, y)].is_ascii_digit() {
                x += 1;
            }
            let end_x = x;

            let num = schematic[(start_x..(end_x + 1), y)]
                .iter()
                .collect::<String>()
                .parse::<i64>()
                .unwrap();

            for x in start_x..=end_x {
                schematic[(x, y)] = '.';
            }

            Some(num)
        };

        for (xo, yo) in &neighbors {
            if let Some(num) = find_number(self, x + xo, y + yo) {
                nums.push(num);
            }
        }
    }
}

impl Index<(i64, i64)> for Schematic {
    type Output = char;
    fn index(&self, (x, y): (i64, i64)) -> &char {
        &self.schematic[y as usize][x as usize]
    }
}

impl Index<(Range<i64>, i64)> for Schematic {
    type Output = [char];
    fn index(&self, (xs, y): (Range<i64>, i64)) -> &[char] {
        &self.schematic[y as usize][(xs.start as usize)..(xs.end as usize)]
    }
}

impl IndexMut<(i64, i64)> for Schematic {
    fn index_mut(&mut self, (x, y): (i64, i64)) -> &mut char {
        &mut self.schematic[y as usize][x as usize]
    }
}

pub fn part1(input: &str) -> u64 {
    let mut schematic = Schematic::parse(input);

    let is_symbol = |c: char| c != '.' && !c.is_ascii_digit();

    let mut nums = Vec::new();
    for y in 0..schematic.h as i64 {
        for x in 0..schematic.w as i64 {
            if is_symbol(schematic[(x, y)]) {
                schematic.find_and_clear_adjacent_numbers(&mut nums, x, y);
            }
        }
    }

    nums.iter().sum::<i64>() as u64
}

pub fn part2(input: &str) -> u64 {
    let schematic = Schematic::parse(input);

    let mut ratios = Vec::new();
    let mut nums = Vec::new();
    for y in 0..schematic.h as i64 {
        for x in 0..schematic.w as i64 {
            if schematic[(x, y)] == '*' {
                schematic
                    .clone()
                    .find_and_clear_adjacent_numbers(&mut nums, x, y);
                if let &[n1, n2] = &nums[..] {
                    ratios.push(n1 * n2);
                }
                nums.clear();
            }
        }
    }

    ratios.iter().sum::<i64>() as u64
}

#[cfg(test)]
mod tests {
    use super::*;

    const EXAMPLE_INPUT: &'static str = "467..114..
...*......
..35..633.
......#...
617*......
.....+.58.
..592.....
......755.
...$.*....
.664.598..
";

    #[test]
    fn p1_example() {
        assert_eq!(part1(EXAMPLE_INPUT), 4361);
    }

    #[test]
    fn p1_input() {
        let input = std::fs::read_to_string("input/day3").expect("reading input file");
        assert_eq!(part1(&input), 525181);
    }

    #[test]
    fn p2_example() {
        assert_eq!(part2(EXAMPLE_INPUT), 467835);
    }

    #[test]
    fn p2_input() {
        let input = std::fs::read_to_string("input/day3").expect("reading input file");
        assert_eq!(part2(&input), 84289137);
    }
}
