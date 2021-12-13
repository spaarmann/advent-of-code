#[derive(Debug, Copy, Clone)]
enum Fold {
    X(usize),
    Y(usize),
}

struct Paper {
    grid: Vec<bool>,
    vec_width: usize,
    width: usize,
    height: usize,
}

fn parse(input: &str) -> (Paper, Vec<Fold>) {
    let (input_points, input_folds) = input.split_once("\n\n").unwrap();

    let mut points = Vec::new();
    let mut max = (0, 0);
    for line in input_points.lines() {
        let (x, y) = line.split_once(',').unwrap();
        let (x, y) = (x.parse().unwrap(), y.parse().unwrap());

        max.0 = usize::max(max.0, x);
        max.1 = usize::max(max.1, y);
        points.push((x, y));
    }

    let (width, height) = (max.0 as usize + 1, max.1 as usize + 1);
    let mut grid = vec![false; width * height];
    for (x, y) in points {
        grid[y * width + x] = true;
    }

    let mut folds = Vec::new();
    for line in input_folds.lines() {
        let (text, num) = line.split_once('=').unwrap();
        let axis = text.chars().last().unwrap();
        let num = num.parse().unwrap();

        folds.push(match axis {
            'x' => Fold::X(num),
            'y' => Fold::Y(num),
            _ => panic!("invalid input"),
        });
    }

    (
        Paper {
            width,
            height,
            grid,
            vec_width: width,
        },
        folds,
    )
}

impl Paper {
    fn apply_fold(&mut self, fold: Fold) {
        let mut new_width = self.width;
        let mut new_height = self.height;

        match fold {
            Fold::X(fold_x) => {
                let min_x = fold_x + 1;
                for y in 0..self.height {
                    for x in min_x..self.width {
                        let new_x = fold_x - (x - fold_x);
                        self.grid[y * self.vec_width + new_x] |= self.grid[y * self.vec_width + x];
                    }
                }
                new_width = fold_x;
            }
            Fold::Y(fold_y) => {
                let min_y = fold_y + 1;
                for y in min_y..self.height {
                    let new_y = fold_y - (y - fold_y);
                    for x in 0..self.width {
                        self.grid[new_y * self.vec_width + x] |= self.grid[y * self.vec_width + x];
                    }
                }
                new_height = fold_y;
            }
        }

        self.width = new_width;
        self.height = new_height;
    }

    fn count_dots(&self) -> u64 {
        let mut count = 0;
        for y in 0..self.height {
            for x in 0..self.width {
                count += self.grid[y * self.vec_width + x] as u64;
            }
        }
        count
    }

    fn to_string(&self) -> String {
        (0..self.height)
            .flat_map(|y| {
                let line_start = y * self.vec_width;
                self.grid[line_start..(line_start + self.width)]
                    .iter()
                    .map(|&p| if p { '█' } else { ' ' })
                    .chain(std::iter::once('\n'))
            })
            .collect()
    }
}

pub fn part1(input: &str) -> u64 {
    let (mut paper, folds) = parse(input);

    paper.apply_fold(*folds.first().unwrap());
    paper.count_dots()
}

pub fn part2(input: &str) -> String {
    let (mut paper, folds) = parse(input);

    for fold in folds {
        paper.apply_fold(fold);
    }

    let mut output = paper.to_string();
    output.insert(0, '\n');
    output
}

#[cfg(test)]
mod tests {
    use super::*;

    const EXAMPLE_INPUT: &'static str = "6,10
0,14
9,10
0,3
10,4
4,11
6,0
6,12
4,1
0,13
10,12
3,4
3,0
8,4
1,10
2,14
8,10
9,0

fold along y=7
fold along x=5
";

    #[test]
    fn p1_example() {
        assert_eq!(part1(EXAMPLE_INPUT), 17);
    }

    #[test]
    fn p1_input() {
        let input = std::fs::read_to_string("input/day13").expect("reading input file");
        assert_eq!(part1(&input), 785);
    }

    const PART2_EXAMPLE_EXPECTED: &'static str = "
█████
█   █
█   █
█   █
█████
     
     
";

    #[test]
    fn p2_example() {
        assert_eq!(part2(EXAMPLE_INPUT), PART2_EXAMPLE_EXPECTED);
    }

    const PART2_INPUT_EXPECTED: &'static str = "
████   ██  ██  █  █   ██  ██   ██  █  █ 
█       █ █  █ █  █    █ █  █ █  █ █  █ 
███     █ █  █ ████    █ █    █  █ ████ 
█       █ ████ █  █    █ █ ██ ████ █  █ 
█    █  █ █  █ █  █ █  █ █  █ █  █ █  █ 
█     ██  █  █ █  █  ██   ███ █  █ █  █ 
";

    #[test]
    fn p2_input() {
        let input = std::fs::read_to_string("input/day13").expect("reading input file");
        assert_eq!(part2(&input), PART2_INPUT_EXPECTED);
    }
}
