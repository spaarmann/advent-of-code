fn parse(input: &str) -> (usize, Vec<u16>) {
    let num_bits = input.lines().next().unwrap().len();

    let nums = input
        .lines()
        .map(|l| u16::from_str_radix(l, 2).unwrap())
        .collect::<Vec<_>>();

    (num_bits, nums)
}

fn count_bits_set(nums: &[u16], bit: usize) -> usize {
    nums.iter().filter(|&&n| (n & (1 << bit)) != 0).count()
}

pub fn part1(input: &str) -> u64 {
    let (num_bits, nums) = parse(input);

    let mut gamma = 0;
    for i in 0..num_bits {
        let bits_set = count_bits_set(&nums, i);
        gamma |= ((bits_set >= nums.len() / 2) as u16) << i;
    }

    let epsilon = ((1 << num_bits) - 1) & !gamma;

    gamma as u64 * epsilon as u64
}

pub fn part2(input: &str) -> u64 {
    let (num_bits, nums) = parse(input);

    let mut oxy_candidates = nums.clone();
    let mut i = (num_bits - 1) as i32;
    while oxy_candidates.len() > 1 {
        if i < 0 {
            panic!("Oxygen filter didn't filter down to only 1 number!");
        }

        let bits_set = count_bits_set(&oxy_candidates, i as usize);
        let bits_unset = oxy_candidates.len() - bits_set;
        let filter_value = ((bits_set >= bits_unset) as u16) << i;
        oxy_candidates.retain(|&n| n & (1 << i) == filter_value);
        i -= 1;
    }

    let mut co2_candidates = nums.clone();
    i = (num_bits - 1) as i32;
    while co2_candidates.len() > 1 {
        if i < 0 {
            panic!("CO2 filter didn't filter down to only 1 number!");
        }

        let bits_set = count_bits_set(&co2_candidates, i as usize);
        let bits_unset = co2_candidates.len() - bits_set;
        let filter_value = ((bits_set < bits_unset) as u16) << i;
        co2_candidates.retain(|&n| n & (1 << i) == filter_value);
        i -= 1;
    }

    let oxy_rating = *oxy_candidates.first().unwrap();
    let co2_rating = *co2_candidates.first().unwrap();

    oxy_rating as u64 * co2_rating as u64
}

#[cfg(test)]
mod tests {
    use super::*;

    const EXAMPLE_INPUT: &'static str = "00100
11110
10110
10111
10101
01111
00111
11100
10000
11001
00010
01010";

    #[test]
    fn p1_example() {
        assert_eq!(part1(EXAMPLE_INPUT), 198);
    }

    #[test]
    fn p1_input() {
        let input = std::fs::read_to_string("input/day3").expect("reading input file");
        assert_eq!(part1(&input), 2972336);
    }

    #[test]
    fn p2_example() {
        assert_eq!(part2(EXAMPLE_INPUT), 230);
    }

    #[test]
    fn p2_input() {
        let input = std::fs::read_to_string("input/day3").expect("reading input file");
        assert_eq!(part2(&input), 3368358);
    }
}
