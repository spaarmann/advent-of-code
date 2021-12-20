fn parse(input: &str) -> (Vec<bool>, Vec<Vec<bool>>) {
    let (algo, image) = input.split_once("\n\n").unwrap();
    let algo = algo.bytes().map(|b| b == b'#').collect();
    let image = image
        .lines()
        .map(|l| l.bytes().map(|b| b == b'#').collect())
        .collect();
    (algo, image)
}

#[derive(Debug, Clone)]
struct Canvas {
    canvas: Vec<Vec<bool>>,
    width: usize,
    height: usize,
}

impl Canvas {
    fn from_input_and_padding(input_image: Vec<Vec<bool>>, padding: usize) -> Self {
        let input_width = input_image[0].len();
        let input_height = input_image.len();

        let canvas_width = input_width + 2 * padding;
        let canvas_height = input_height + 2 * padding;
        let mut canvas = Vec::new();
        for _ in 0..padding {
            canvas.push(vec![false; canvas_width]);
        }
        canvas.extend(input_image.into_iter().map(|mut v| {
            for _ in 0..padding {
                v.insert(0, false);
                v.push(false);
            }
            v
        }));
        for _ in 0..padding {
            canvas.push(vec![false; canvas_width]);
        }

        Self {
            canvas,
            width: canvas_width,
            height: canvas_height,
        }
    }

    fn get(&self, x: isize, y: isize) -> bool {
        let x = x.clamp(0, self.width as isize - 1) as usize;
        let y = y.clamp(0, self.height as isize - 1) as usize;
        self.canvas[y][x]
    }

    fn set(&mut self, x: isize, y: isize, val: bool) {
        if x >= 0 && x < self.width as isize && y >= 0 && y < self.height as isize {
            self.canvas[y as usize][x as usize] = val;
        }
    }
}

fn enhance(input: &str, rounds: usize) -> u64 {
    let (algo, input_image) = parse(input);

    let mut canvas = Canvas::from_input_and_padding(input_image, rounds);

    for _ in 0..rounds {
        let mut output = canvas.clone();
        for y in 0..(canvas.height as isize) {
            for x in 0..(canvas.width as isize) {
                let bits = [
                    (-1, -1),
                    (0, -1),
                    (1, -1),
                    (-1, 0),
                    (0, 0),
                    (1, 0),
                    (-1, 1),
                    (0, 1),
                    (1, 1),
                ]
                .into_iter()
                .map(|(off_x, off_y)| canvas.get(x + off_x, y + off_y));

                let num = bits.fold(0, |acc, b| (acc << 1) | (b as usize));

                output.set(x, y, algo[num]);
            }
        }
        canvas = output;
    }

    canvas
        .canvas
        .iter()
        .map(|l| l.iter().filter(|&&b| b).count() as u64)
        .sum::<u64>()
}

pub fn part1(input: &str) -> u64 {
    enhance(input, 2)
}

pub fn part2(input: &str) -> u64 {
    enhance(input, 50)
}

#[cfg(test)]
mod tests {
    use super::*;

    const EXAMPLE_INPUT: &'static str = "..#.#..#####.#.#.#.###.##.....###.##.#..###.####..#####..#....#..#..##..###..######.###...####..#..#####..##..#.#####...##.#.#..#.##..#.#......#.###.######.###.####...#.##.##..#..#..#####.....#.#....###..#.##......#.....#..#..#..##..#...##.######.####.####.#.#...#.......#..#.#.#...####.##.#......#..#...##.#.##..#...##.#.##..###.#......#.#.......#.#.#.####.###.##...#.....####.#..#..#.##.#....##..#.####....##...##..#...#......#.#.......#.......##..####..#...#.#.#...##..#.#..###..#####........#..####......#..#

#..#.
#....
##..#
..#..
..###
";

    #[test]
    fn p1_example() {
        assert_eq!(part1(EXAMPLE_INPUT), 35);
    }

    #[test]
    fn p1_input() {
        let input = std::fs::read_to_string("input/day20").expect("reading input file");
        assert_eq!(part1(&input), 5065);
    }

    #[test]
    fn p2_example() {
        assert_eq!(part2(EXAMPLE_INPUT), 3351);
    }

    #[test]
    fn p2_input() {
        if std::env::var("RUN_SLOW_TESTS")
            .map(|s| s != "0")
            .unwrap_or(false)
        {
            let input = std::fs::read_to_string("input/day20").expect("reading input file");
            assert_eq!(part2(&input), 14790);
        }
    }
}
