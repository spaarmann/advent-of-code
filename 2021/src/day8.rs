use std::{
    collections::{HashMap, HashSet},
    iter::FromIterator,
};

use itertools::Itertools;

pub fn part1(input: &str) -> u64 {
    input
        .lines()
        .map(|line| line.split_once('|').unwrap().1.split_whitespace())
        .flatten()
        .filter(|digits| {
            let len = digits.len();
            len == 2 || len == 3 || len == 4 || len == 7
        })
        .count() as u64
}

pub fn part2(input: &str) -> u64 {
    let map = HashMap::<char, usize>::from([
        ('a', 0),
        ('b', 1),
        ('c', 2),
        ('d', 3),
        ('e', 4),
        ('f', 5),
        ('g', 6),
    ]);

    let segment_displays: [&[char]; 10] = [
        /* 0 */ &['a', 'b', 'c', 'e', 'f', 'g'],
        /* 1 */ &['c', 'f'],
        /* 2 */ &['a', 'c', 'd', 'e', 'g'],
        /* 3 */ &['a', 'c', 'd', 'f', 'g'],
        /* 4 */ &['b', 'c', 'd', 'f'],
        /* 5 */ &['a', 'b', 'd', 'f', 'g'],
        /* 6 */ &['a', 'b', 'd', 'e', 'f', 'g'],
        /* 7 */ &['a', 'c', 'f'],
        /* 8 */ &['a', 'b', 'c', 'd', 'e', 'f', 'g'],
        /* 9 */ &['a', 'b', 'c', 'd', 'f', 'g'],
    ];

    let segment_display_sets =
        segment_displays.map(|segments| HashSet::<char>::from_iter(segments.iter().copied()));

    let segment_display_sets_mapped = segment_displays
        .map(|segments| HashSet::<usize>::from_iter(segments.iter().map(|s| map[s])));

    let segment_diffs = segment_display_sets
        .iter()
        .enumerate()
        .permutations(2)
        .filter_map(|v| {
            let [(i1, segs1), (i2, segs2)] = v.as_slice() else { panic!("should be two-element vec!") };
            let diff = segs1.difference(segs2).copied().collect_vec();
            let other_diff = segs2.difference(segs1).copied().collect_vec();

            // TODO: Is this more restrictive than it needs to be?
            if other_diff.len() == 0 {
                Some(((*i1, *i2), diff))
            } else {
                None
            }
        })
        .collect::<HashMap<(usize, usize), Vec<char>>>();

    let input: Vec<(Vec<Vec<usize>>, Vec<Vec<usize>>)> = input
        .lines()
        .map(|line| line.split_once('|').unwrap())
        .map(|(signals, outputs)| {
            (
                signals
                    .split_whitespace()
                    .map(|signal_block| signal_block.chars().map(|c| map[&c]).collect_vec())
                    .collect_vec(),
                outputs
                    .split_whitespace()
                    .map(|output_digit| output_digit.chars().map(|c| map[&c]).collect_vec())
                    .collect_vec(),
            )
        })
        .collect();

    let numbers_with_unique_lengths = [1, 4, 7, 8];

    let mut decoded_outputs = Vec::new();

    for (signals, outputs) in input.into_iter() {
        let signals = {
            let mut signals = signals.clone();
            signals.append(&mut outputs.clone());
            signals
        };

        // At the start, every signal wire could correspond to each segment
        let mut possibilities = [[true; 7]; 7];

        // First, take advantage of the unique-length signals to eliminate some possibilities:
        for signal in &signals {
            let possible_segments: &[char] = match signal.len() {
                2 => {
                    /* 1 */
                    &['c', 'f']
                }
                3 => {
                    /* 7 */
                    &['a', 'c', 'f']
                }
                4 => {
                    /* 4 */
                    &['b', 'c', 'd', 'f']
                }
                7 => {
                    /* 8 */
                    // TODO: This is useless?
                    &['a', 'b', 'c', 'd', 'e', 'f', 'g']
                }
                _ => {
                    /* not unique */
                    continue;
                }
            };

            let possible_segments = possible_segments.iter().map(|c| map[&c]).collect_vec();

            // Every wire in the signal must correspond to one of the segments in the slice.
            // Remove every other possibility.
            for &wire in signal {
                for (segment, flag) in possibilities[wire].iter_mut().enumerate() {
                    if !possible_segments.contains(&segment) {
                        *flag = false;
                    }
                }
            }
        }

        println!("Possibilities after unique lengths: {:?}", possibilities);

        // Map wire to segment
        let mut solution_map = [None; 7];
        // Map segment to wire
        let mut inv_solution_map = [None; 7];

        while !solution_map.iter().all(|e| e.is_some()) {
            for ((i1, i2), segment_diff) in &segment_diffs {
                // Example: (7, 1), ['a']

                if !numbers_with_unique_lengths.contains(i1)
                    || !numbers_with_unique_lengths.contains(i2)
                {
                    continue;
                }

                // For now, only do something with one-element diffs.
                // TODO: We could do more here, if we already have some wires we can 100% map
                let [diff] = segment_diff.as_slice() else { continue; };

                let segments1 = segment_displays[*i1];
                let segments2 = segment_displays[*i2];

                for signal1 in &signals {
                    if signal1.len() != segments1.len() {
                        continue;
                    }

                    for signal2 in &signals {
                        if signal2.len() != segments2.len() {
                            continue;
                        }

                        // Continuing the example, we could e.g. have signal1 [cab] and signal2 [ab].
                        // Taking the diff of that, we get 'c'. We can thus conclude that 'c' must
                        // correspond to `diff`!

                        let signal_diff = HashSet::<usize>::from_iter(signal1.iter().copied())
                            .difference(&HashSet::from_iter(signal2.iter().copied()))
                            .copied()
                            .collect_vec();

                        // Again, 1-length diffs only for now.
                        let [signal_diff] = signal_diff.as_slice() else { continue; };

                        println!("Diffs: Got {} -> {}!", signal_diff, diff);
                        //dbg!((i1, i2));
                        //dbg!((signal1, signal2));

                        // Signal wire `signal_diff` must map to segment `diff`!
                        for (segment, flag) in possibilities[*signal_diff].iter_mut().enumerate() {
                            if segment != map[diff] {
                                *flag = false;
                            } else {
                                *flag = true;
                            }
                        }
                    }
                }
            }

            println!("Possibilities after diffs: {:?}", possibilities);
            assert!(possibilities
                .iter()
                .any(|p| p.iter().filter(|b| **b).count() == 1));

            let cf_candidates = possibilities
                .iter()
                .enumerate()
                .filter(|(_, flags)| {
                    flags
                        .iter()
                        .enumerate()
                        .all(|(i, f)| *f == (i == map[&'c'] || i == map[&'f']))
                })
                //.filter(|(_, flags)| flags[map[&'c']] && flags[map[&'f']])
                .map(|(i, _)| i)
                .collect_vec();

            if let [candidate1, candidate2] = cf_candidates.as_slice() {
                println!("cf_candidates: [{}, {}]", candidate1, candidate2);

                let length6_signals = signals.iter().filter(|s| s.len() == 6);
                for sig in length6_signals {
                    let mut cf = None;
                    if sig.contains(candidate1) && !sig.contains(candidate2) {
                        // candidate1 -> f && candidate2 -> c
                        cf = Some((*candidate2, *candidate1));
                    } else if sig.contains(candidate2) && !sig.contains(candidate1) {
                        // candidate2 -> f && candidate1 -> c
                        cf = Some((*candidate1, *candidate2));
                    }

                    if let Some((sig_c, sig_f)) = cf {
                        for (segment, flag) in possibilities[sig_c].iter_mut().enumerate() {
                            if segment == map[&'c'] {
                                *flag = true;
                            } else {
                                *flag = false;
                            }
                        }
                        for (segment, flag) in possibilities[sig_f].iter_mut().enumerate() {
                            if segment == map[&'f'] {
                                *flag = true;
                            } else {
                                *flag = false;
                            }
                        }

                        println!("Found {} -> c and {} -> f!", sig_c, sig_f);
                    }
                }
            }

            if let (Some(wire_a), Some(wire_c), Some(wire_f)) = (
                inv_solution_map[map[&'a']],
                inv_solution_map[map[&'c']],
                inv_solution_map[map[&'f']],
            ) {
                let length5_signals = signals.iter().filter(|s| s.len() == 5);
                for sig in length5_signals {
                    let (known_missing, other_missing_segment) = if !sig.contains(&wire_c) {
                        // sig must be a 5, and the other missing wire is 'e'!
                        (wire_c, 'e')
                    } else if !sig.contains(&wire_f) {
                        // sig must be a 2, and the other missing wire is 'b'!
                        (wire_f, 'b')
                    } else {
                        continue;
                    };

                    let missing_wires = (0..7).filter(|w| !sig.contains(w)).collect_vec();
                    let [missing1, missing2] = missing_wires.as_slice() else { panic!("too many missing!") };
                    let other_missing = if *missing1 == known_missing {
                        *missing2
                    } else if *missing2 == known_missing {
                        *missing1
                    } else {
                        panic!("two unknown missing!")
                    };

                    println!(
                        "5-length: Found {} -> {}",
                        other_missing, other_missing_segment
                    );

                    for (segment, flag) in possibilities[other_missing].iter_mut().enumerate() {
                        if segment == map[&other_missing_segment] {
                            *flag = true;
                        } else {
                            *flag = false;
                        }
                    }
                }
            }

            // Check if there are any more signals we can fully decide now, and mark them off if
            // so.
            for wire in 0..7 {
                // Already handled this one!
                if solution_map[wire].is_some() {
                    continue;
                }

                if let [(segment, _)] = possibilities[wire]
                    .iter()
                    .enumerate()
                    .filter(|(_, p)| **p)
                    .collect_vec()
                    .as_slice()
                {
                    let segment = *segment;
                    solution_map[wire] = Some(segment);
                    inv_solution_map[segment] = Some(wire);

                    println!(
                        "Found {} -> {}, current possibilities: {:?}",
                        wire, segment, possibilities
                    );

                    for other_wire in 0..7 {
                        if solution_map[other_wire].is_some() {
                            continue;
                        }
                        if other_wire == wire {
                            continue;
                        }

                        for (seg, flag) in possibilities[other_wire].iter_mut().enumerate() {
                            if seg == segment {
                                *flag = false;
                            }
                        }
                    }

                    println!("After updating possibilities: {:?}", possibilities);
                }
            }
        }

        println!("Solution Map: {:?}", solution_map);

        let output_string = String::from_iter(outputs.iter().map(|out_signal| {
            let set =
                HashSet::<usize>::from_iter(out_signal.iter().map(|&s| solution_map[s].unwrap()));
            segment_display_sets_mapped
                .iter()
                .enumerate()
                .find(|(_, seg_set)| set == **seg_set)
                .unwrap()
                .0
                .to_string()
        }));
        decoded_outputs.push(output_string);
    }

    dbg!(&decoded_outputs);

    decoded_outputs
        .iter()
        .map(|o| o.parse::<u64>().unwrap())
        .sum()
}

#[cfg(test)]
mod tests {
    use super::*;

    const EXAMPLE_INPUT: &'static str =
        "be cfbegad cbdgef fgaecd cgeb fdcge agebfd fecdb fabcd edb | fdgacbe cefdb cefbgd gcbe
edbfga begcd cbg gc gcadebf fbgde acbgfd abcde gfcbed gfec | fcgedb cgb dgebacf gc
fgaebd cg bdaec gdafb agbcfd gdcbef bgcad gfac gcb cdgabef | cg cg fdcagb cbg
fbegcd cbd adcefb dageb afcb bc aefdc ecdab fgdeca fcdbega | efabcd cedba gadfec cb
aecbfdg fbg gf bafeg dbefa fcge gcbea fcaegb dgceab fcbdga | gecf egdcabf bgf bfgea
fgeab ca afcebg bdacfeg cfaedg gcfdb baec bfadeg bafgc acf | gebdcfa ecba ca fadegcb
dbcfg fgd bdegcaf fgec aegbdf ecdfab fbedc dacgb gdcebf gf | cefg dcbef fcge gbcadfe
bdfegc cbegaf gecbf dfcage bdacg ed bedf ced adcbefg gebcd | ed bcgafe cdgba cbgef
egadfb cdbfeg cegd fecab cgb gbdefca cg fgcdab egfdb bfceg | gbdfcae bgc cg cgb
gcafb gcf dcaebfg ecagb gf abcdeg gaef cafbge fdbac fegbdc | fgae cfgab fg bagce
";

    #[test]
    fn p1_example() {
        assert_eq!(part1(EXAMPLE_INPUT), 26);
    }

    #[test]
    fn p1_input() {
        let input = std::fs::read_to_string("input/day8").expect("reading input file");
        assert_eq!(part1(&input), 381);
    }

    #[test]
    fn p2_example() {
        assert_eq!(part2(EXAMPLE_INPUT), 61229);
    }

    #[test]
    fn p2_input() {
        let input = std::fs::read_to_string("input/day8").expect("reading input file");
        assert_eq!(part2(&input), 1023686);
    }
}
