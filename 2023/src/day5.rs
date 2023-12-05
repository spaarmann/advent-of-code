use itertools::Itertools;

type Almanac = (Vec<i64>, Vec<Map>);
type Map = Vec<ConvertRange>;

struct ConvertRange {
    dst: i64,
    src: i64,
    len: i64,
}

fn parse(input: &str) -> Almanac {
    let mut blocks = input.split("\n\n");

    let seeds = blocks
        .next()
        .unwrap()
        .split_once(": ")
        .unwrap()
        .1
        .split_whitespace()
        .map(|n| n.parse::<i64>().unwrap())
        .collect_vec();

    let maps = blocks
        .map(|b| {
            let lines = b.lines().skip(1);
            lines
                .map(|l| {
                    let mut nums = l.split_whitespace().map(|n| n.parse::<i64>().unwrap());
                    ConvertRange {
                        dst: nums.next().unwrap(),
                        src: nums.next().unwrap(),
                        len: nums.next().unwrap(),
                    }
                })
                .collect_vec()
        })
        .collect_vec();

    (seeds, maps)
}

pub fn part1(input: &str) -> u64 {
    let (seeds, maps) = parse(input);

    seeds
        .into_iter()
        .map(|seed| {
            let mut current_num = seed;
            'maps: for map in &maps {
                for range in map {
                    if current_num >= range.src && current_num < range.src + range.len {
                        current_num = range.dst + (current_num - range.src);
                        continue 'maps;
                    }
                }
            }
            current_num
        })
        .min()
        .unwrap() as u64
}

pub fn part2(input: &str) -> u64 {
    // I guess do it for whole intervals, splitting them up everytime they cross a mapping boundary?
    // If that happens? (Probably)
    todo!()
}

#[cfg(test)]
mod tests {
    use super::*;

    const EXAMPLE_INPUT: &'static str = "seeds: 79 14 55 13

seed-to-soil map:
50 98 2
52 50 48

soil-to-fertilizer map:
0 15 37
37 52 2
39 0 15

fertilizer-to-water map:
49 53 8
0 11 42
42 0 7
57 7 4

water-to-light map:
88 18 7
18 25 70

light-to-temperature map:
45 77 23
81 45 19
68 64 13

temperature-to-humidity map:
0 69 1
1 0 69

humidity-to-location map:
60 56 37
56 93 4
";

    #[test]
    fn p1_example() {
        assert_eq!(part1(EXAMPLE_INPUT), 35);
    }

    #[test]
    fn p1_input() {
        let input = std::fs::read_to_string("input/day5").expect("reading input file");
        assert_eq!(part1(&input), 910845529);
    }

    #[test]
    fn p2_example() {
        assert_eq!(part2(EXAMPLE_INPUT), 46);
    }

    #[test]
    fn p2_input() {
        let input = std::fs::read_to_string("input/day5").expect("reading input file");
        assert_eq!(part2(&input), todo!());
    }
}
