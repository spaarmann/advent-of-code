use std::{
    collections::{HashMap, HashSet},
    ops::{Add, Mul, Sub},
};

use itertools::Itertools;

#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
struct Vec3 {
    x: i64,
    y: i64,
    z: i64,
}

impl Vec3 {
    const fn new(x: i64, y: i64, z: i64) -> Self {
        Self { x, y, z }
    }

    const fn cross(self, rhs: Self) -> Self {
        Self::new(
            self.y * rhs.z - self.z * rhs.y,
            self.z * rhs.x - self.x * rhs.z,
            self.x * rhs.y - self.y * rhs.x,
        )
    }
}

impl Add<Vec3> for Vec3 {
    type Output = Vec3;

    fn add(self, rhs: Vec3) -> Self::Output {
        Vec3::new(self.x + rhs.x, self.y + rhs.y, self.z + rhs.z)
    }
}

impl Sub<Vec3> for Vec3 {
    type Output = Vec3;

    fn sub(self, rhs: Vec3) -> Self::Output {
        Vec3::new(self.x - rhs.x, self.y - rhs.y, self.z - rhs.z)
    }
}

impl Mul<Vec3> for i64 {
    type Output = Vec3;

    fn mul(self, rhs: Vec3) -> Self::Output {
        Vec3::new(rhs.x * self, rhs.y * self, rhs.z * self)
    }
}

#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
struct Orientation {
    up: Vec3,
    right: Vec3,
    forward: Vec3,
}

const ORIENTATIONS: [Orientation; 24] = [
    Orientation::new(Vec3::new(0, 1, 0), Vec3::new(1, 0, 0)),
    Orientation::new(Vec3::new(0, 1, 0), Vec3::new(-1, 0, 0)),
    Orientation::new(Vec3::new(0, 1, 0), Vec3::new(0, 0, 1)),
    Orientation::new(Vec3::new(0, 1, 0), Vec3::new(0, 0, -1)),
    Orientation::new(Vec3::new(0, -1, 0), Vec3::new(1, 0, 0)),
    Orientation::new(Vec3::new(0, -1, 0), Vec3::new(-1, 0, 0)),
    Orientation::new(Vec3::new(0, -1, 0), Vec3::new(0, 0, 1)),
    Orientation::new(Vec3::new(0, -1, 0), Vec3::new(0, 0, -1)),
    Orientation::new(Vec3::new(1, 0, 0), Vec3::new(0, 1, 0)),
    Orientation::new(Vec3::new(1, 0, 0), Vec3::new(0, -1, 0)),
    Orientation::new(Vec3::new(1, 0, 0), Vec3::new(0, 0, 1)),
    Orientation::new(Vec3::new(1, 0, 0), Vec3::new(0, 0, -1)),
    Orientation::new(Vec3::new(-1, 0, 0), Vec3::new(0, 1, 0)),
    Orientation::new(Vec3::new(-1, 0, 0), Vec3::new(0, -1, 0)),
    Orientation::new(Vec3::new(-1, 0, 0), Vec3::new(0, 0, 1)),
    Orientation::new(Vec3::new(-1, 0, 0), Vec3::new(0, 0, -1)),
    Orientation::new(Vec3::new(0, 0, 1), Vec3::new(0, 1, 0)),
    Orientation::new(Vec3::new(0, 0, 1), Vec3::new(0, -1, 0)),
    Orientation::new(Vec3::new(0, 0, 1), Vec3::new(1, 0, 0)),
    Orientation::new(Vec3::new(0, 0, 1), Vec3::new(-1, 0, 0)),
    Orientation::new(Vec3::new(0, 0, -1), Vec3::new(0, 1, 0)),
    Orientation::new(Vec3::new(0, 0, -1), Vec3::new(0, -1, 0)),
    Orientation::new(Vec3::new(0, 0, -1), Vec3::new(1, 0, 0)),
    Orientation::new(Vec3::new(0, 0, -1), Vec3::new(-1, 0, 0)),
];

impl Orientation {
    const fn new(up: Vec3, right: Vec3) -> Self {
        Self {
            up,
            right,
            forward: right.cross(up),
        }
    }

    fn transform(&self, p: Vec3) -> Vec3 {
        p.x * self.right + p.y * self.up + p.z * self.forward
    }
}

fn parse_point(line: &str) -> Vec3 {
    let mut components = line.split(',');
    Vec3::new(
        components.next().unwrap().parse().unwrap(),
        components.next().unwrap().parse().unwrap(),
        components.next().unwrap().parse().unwrap(),
    )
}

fn parse(input: &str) -> Vec<Vec<Vec3>> {
    input
        .split("\n\n")
        .map(|block| block.lines().skip(1).map(parse_point).collect())
        .collect()
}

fn try_match(
    first_offset: Vec3,
    first_beacons: &[Vec3],
    second_beacons: &[Vec3],
) -> Option<(Orientation, Vec3, Vec<Vec3>)> {
    for assume_match_first in 0..first_beacons.len() {
        let assume_first = first_beacons[assume_match_first];

        for orientation in &ORIENTATIONS {
            for assume_match_second in 0..second_beacons.len() {
                let assume_second = orientation.transform(second_beacons[assume_match_second]);

                // Assume `assume_first` and `assume_second` were the same beacon.
                // Would we have 12+ overlaps in that case?
                let assumed_offset = assume_first - assume_second;
                let second_transformed_to_first: Vec<_> = second_beacons
                    .iter()
                    .map(|&p| orientation.transform(p) + assumed_offset)
                    .collect();
                let second_within_first_range = second_transformed_to_first.iter().filter(|&&p| {
                    let p = p - first_offset;
                    p.x >= -1000
                        && p.x <= 1000
                        && p.y >= -1000
                        && p.y <= 1000
                        && p.z >= -1000
                        && p.z <= 1000
                });
                let second_also_in_first =
                    second_within_first_range.filter(|p| first_beacons.contains(p));

                if second_also_in_first.count() >= 12 {
                    return Some((*orientation, assumed_offset, second_transformed_to_first));
                }
            }
        }
    }

    None
}

fn solve(input: &str) -> HashMap<usize, (Vec3, Vec<Vec3>)> {
    let measurements = parse(input);

    let mut solutions = HashMap::from([(0, (Vec3::new(0, 0, 0), measurements[0].clone()))]);
    let mut solved_scanners_to_try = vec![0];
    let mut unsolved_scanners: Vec<_> = (1..measurements.len()).collect();

    while unsolved_scanners.len() > 0 {
        let scanner = solved_scanners_to_try
            .pop()
            .expect("still has unsolved scanners but no solved one to try next!");

        let mut new_solved = Vec::new();
        for i in 0..unsolved_scanners.len() {
            let scanner_solution = &solutions[&scanner];
            let unsolved = unsolved_scanners[i];
            if let Some((_, offset, transformed)) = try_match(
                scanner_solution.0,
                &scanner_solution.1,
                &measurements[unsolved],
            ) {
                println!("Matched {} with {}!", unsolved, scanner);
                solutions.insert(unsolved, (offset, transformed));
                solved_scanners_to_try.push(unsolved);
                new_solved.push(unsolved);
            }
        }

        unsolved_scanners.retain(|unsolved| !new_solved.contains(unsolved));
    }

    solutions
}

pub fn part1(input: &str) -> u64 {
    let solutions = solve(input);
    let mut beacons = HashSet::new();
    for (_, (_, transformed_measurements)) in solutions {
        beacons.extend(transformed_measurements);
    }

    beacons.len() as u64
}

pub fn part2(input: &str) -> u64 {
    let solutions = solve(input);
    let scanner_positions: Vec<_> = solutions.into_values().map(|(p, _)| p).collect();

    scanner_positions
        .into_iter()
        .tuple_combinations()
        .map(|(p1, p2)| (p1.x - p2.x).abs() + (p1.y - p2.y).abs() + (p1.z - p2.z).abs())
        .max()
        .unwrap() as u64
}

#[cfg(test)]
mod tests {
    use super::*;

    const EXAMPLE_INPUT: &'static str = "--- scanner 0 ---
404,-588,-901
528,-643,409
-838,591,734
390,-675,-793
-537,-823,-458
-485,-357,347
-345,-311,381
-661,-816,-575
-876,649,763
-618,-824,-621
553,345,-567
474,580,667
-447,-329,318
-584,868,-557
544,-627,-890
564,392,-477
455,729,728
-892,524,684
-689,845,-530
423,-701,434
7,-33,-71
630,319,-379
443,580,662
-789,900,-551
459,-707,401

--- scanner 1 ---
686,422,578
605,423,415
515,917,-361
-336,658,858
95,138,22
-476,619,847
-340,-569,-846
567,-361,727
-460,603,-452
669,-402,600
729,430,532
-500,-761,534
-322,571,750
-466,-666,-811
-429,-592,574
-355,545,-477
703,-491,-529
-328,-685,520
413,935,-424
-391,539,-444
586,-435,557
-364,-763,-893
807,-499,-711
755,-354,-619
553,889,-390

--- scanner 2 ---
649,640,665
682,-795,504
-784,533,-524
-644,584,-595
-588,-843,648
-30,6,44
-674,560,763
500,723,-460
609,671,-379
-555,-800,653
-675,-892,-343
697,-426,-610
578,704,681
493,664,-388
-671,-858,530
-667,343,800
571,-461,-707
-138,-166,112
-889,563,-600
646,-828,498
640,759,510
-630,509,768
-681,-892,-333
673,-379,-804
-742,-814,-386
577,-820,562

--- scanner 3 ---
-589,542,597
605,-692,669
-500,565,-823
-660,373,557
-458,-679,-417
-488,449,543
-626,468,-788
338,-750,-386
528,-832,-391
562,-778,733
-938,-730,414
543,643,-506
-524,371,-870
407,773,750
-104,29,83
378,-903,-323
-778,-728,485
426,699,580
-438,-605,-362
-469,-447,-387
509,732,623
647,635,-688
-868,-804,481
614,-800,639
595,780,-596

--- scanner 4 ---
727,592,562
-293,-554,779
441,611,-461
-714,465,-776
-743,427,-804
-660,-479,-426
832,-632,460
927,-485,-438
408,393,-506
466,436,-512
110,16,151
-258,-428,682
-393,719,612
-211,-452,876
808,-476,-593
-575,615,604
-485,667,467
-680,325,-822
-627,-443,-432
872,-547,-609
833,512,582
807,604,487
839,-516,451
891,-625,532
-652,-548,-490
30,-46,-14
";

    #[test]
    fn p1_example() {
        assert_eq!(part1(EXAMPLE_INPUT), 79);
    }

    #[test]
    fn p1_input() {
        if std::env::var("RUN_SLOW_TESTS")
            .map(|s| s != "0")
            .unwrap_or(false)
        {
            let input = std::fs::read_to_string("input/day19").expect("reading input file");
            assert_eq!(part1(&input), 479);
        }
    }

    #[test]
    fn p2_example() {
        assert_eq!(part2(EXAMPLE_INPUT), 3621);
    }

    #[test]
    fn p2_input() {
        if std::env::var("RUN_SLOW_TESTS")
            .map(|s| s != "0")
            .unwrap_or(false)
        {
            let input = std::fs::read_to_string("input/day19").expect("reading input file");
            assert_eq!(part2(&input), 13113);
        }
    }
}
