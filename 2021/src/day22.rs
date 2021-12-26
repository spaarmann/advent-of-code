#[derive(Debug, Copy, Clone)]
struct Point {
    x: i64,
    y: i64,
    z: i64,
}

impl Point {
    fn new(x: i64, y: i64, z: i64) -> Self {
        Self { x, y, z }
    }

    fn min(self, other: Self) -> Self {
        Self::new(
            self.x.min(other.x),
            self.y.min(other.y),
            self.z.min(other.z),
        )
    }

    fn max(self, other: Self) -> Self {
        Self::new(
            self.x.max(other.x),
            self.y.max(other.y),
            self.z.max(other.z),
        )
    }
}

#[derive(Debug, Copy, Clone)]
struct Cuboid {
    min: Point,
    max: Point,
}

impl Cuboid {
    fn intersection(self, other: Self) -> Option<Self> {
        if self.min.x >= other.max.x
            || self.min.y >= other.max.y
            || self.min.z >= other.max.z
            || self.max.x <= other.min.x
            || self.max.y <= other.min.y
            || self.max.z <= other.min.z
        {
            None
        } else {
            let min = self.min.max(other.min);
            let max = self.max.min(other.max);
            Some(Self { min, max })
        }
    }

    fn volume(self) -> i64 {
        (self.max.x - self.min.x) * (self.max.y - self.min.y) * (self.max.z - self.min.z)
    }
}

#[derive(Debug)]
struct Step {
    value: bool,
    area: Cuboid,
}

fn parse(input: &str) -> impl Iterator<Item = Step> + '_ {
    input.lines().map(|l| {
        let (value, range) = l.split_once(" ").unwrap();
        let value = match value {
            "on" => true,
            "off" => false,
            _ => panic!("unknown value: {}", value),
        };

        let mut components = range
            .split(",")
            .map(|s| s.split_once("=").unwrap().1.split_once("..").unwrap());

        let (x_min, x_max) = components.next().unwrap();
        let (y_min, y_max) = components.next().unwrap();
        let (z_min, z_max) = components.next().unwrap();

        let min = Point::new(
            x_min.parse().unwrap(),
            y_min.parse().unwrap(),
            z_min.parse().unwrap(),
        );
        let max = Point::new(
            x_max.parse::<i64>().unwrap() + 1,
            y_max.parse::<i64>().unwrap() + 1,
            z_max.parse::<i64>().unwrap() + 1,
        );

        Step {
            value,
            area: Cuboid { min, max },
        }
    })
}

fn perform_reboot(steps: impl Iterator<Item = Step>) -> u64 {
    let mut positive_volumes = Vec::<Cuboid>::new();
    let mut negative_volumes = Vec::<Cuboid>::new();

    let mut new_positive = Vec::new();
    let mut new_negative = Vec::new();
    for step in steps {
        let positive_intersections = positive_volumes
            .iter()
            .filter_map(|c| c.intersection(step.area));
        let negative_intersections = negative_volumes
            .iter()
            .filter_map(|c| c.intersection(step.area));

        for i in positive_intersections {
            new_negative.push(i);
        }
        for i in negative_intersections {
            new_positive.push(i);
        }

        if step.value {
            new_positive.push(step.area);
        }

        positive_volumes.extend(new_positive.iter());
        negative_volumes.extend(new_negative.iter());

        new_positive.clear();
        new_negative.clear();
    }

    (positive_volumes
        .into_iter()
        .map(|c| c.volume())
        .sum::<i64>()
        - negative_volumes
            .into_iter()
            .map(|c| c.volume())
            .sum::<i64>()) as _
}

pub fn part1(input: &str) -> u64 {
    let steps = parse(input).filter(|s| {
        s.area.min.x >= -50
            && s.area.min.y >= -50
            && s.area.min.z >= -50
            && s.area.max.x <= 51
            && s.area.max.y <= 51
            && s.area.max.z <= 51
    });
    perform_reboot(steps)
}

pub fn part2(input: &str) -> u64 {
    perform_reboot(parse(input))
}

#[cfg(test)]
mod tests {
    use super::*;

    const EXAMPLE1_INPUT: &'static str = "on x=10..12,y=10..12,z=10..12
on x=11..13,y=11..13,z=11..13
off x=9..11,y=9..11,z=9..11
on x=10..10,y=10..10,z=10..10
";

    const EXAMPLE2_INPUT: &'static str = "on x=-20..26,y=-36..17,z=-47..7
on x=-20..33,y=-21..23,z=-26..28
on x=-22..28,y=-29..23,z=-38..16
on x=-46..7,y=-6..46,z=-50..-1
on x=-49..1,y=-3..46,z=-24..28
on x=2..47,y=-22..22,z=-23..27
on x=-27..23,y=-28..26,z=-21..29
on x=-39..5,y=-6..47,z=-3..44
on x=-30..21,y=-8..43,z=-13..34
on x=-22..26,y=-27..20,z=-29..19
off x=-48..-32,y=26..41,z=-47..-37
on x=-12..35,y=6..50,z=-50..-2
off x=-48..-32,y=-32..-16,z=-15..-5
on x=-18..26,y=-33..15,z=-7..46
off x=-40..-22,y=-38..-28,z=23..41
on x=-16..35,y=-41..10,z=-47..6
off x=-32..-23,y=11..30,z=-14..3
on x=-49..-5,y=-3..45,z=-29..18
off x=18..30,y=-20..-8,z=-3..13
on x=-41..9,y=-7..43,z=-33..15
on x=-54112..-39298,y=-85059..-49293,z=-27449..7877
on x=967..23432,y=45373..81175,z=27513..53682
";
    #[test]
    fn p1_example1() {
        assert_eq!(part1(EXAMPLE1_INPUT), 39);
    }

    #[test]
    fn p1_example2() {
        assert_eq!(part1(EXAMPLE2_INPUT), 590784);
    }

    #[test]
    fn p1_input() {
        let input = std::fs::read_to_string("input/day22").expect("reading input file");
        assert_eq!(part1(&input), 581108);
    }

    const EXAMPLE3_INPUT: &'static str = "on x=-5..47,y=-31..22,z=-19..33
on x=-44..5,y=-27..21,z=-14..35
on x=-49..-1,y=-11..42,z=-10..38
on x=-20..34,y=-40..6,z=-44..1
off x=26..39,y=40..50,z=-2..11
on x=-41..5,y=-41..6,z=-36..8
off x=-43..-33,y=-45..-28,z=7..25
on x=-33..15,y=-32..19,z=-34..11
off x=35..47,y=-46..-34,z=-11..5
on x=-14..36,y=-6..44,z=-16..29
on x=-57795..-6158,y=29564..72030,z=20435..90618
on x=36731..105352,y=-21140..28532,z=16094..90401
on x=30999..107136,y=-53464..15513,z=8553..71215
on x=13528..83982,y=-99403..-27377,z=-24141..23996
on x=-72682..-12347,y=18159..111354,z=7391..80950
on x=-1060..80757,y=-65301..-20884,z=-103788..-16709
on x=-83015..-9461,y=-72160..-8347,z=-81239..-26856
on x=-52752..22273,y=-49450..9096,z=54442..119054
on x=-29982..40483,y=-108474..-28371,z=-24328..38471
on x=-4958..62750,y=40422..118853,z=-7672..65583
on x=55694..108686,y=-43367..46958,z=-26781..48729
on x=-98497..-18186,y=-63569..3412,z=1232..88485
on x=-726..56291,y=-62629..13224,z=18033..85226
on x=-110886..-34664,y=-81338..-8658,z=8914..63723
on x=-55829..24974,y=-16897..54165,z=-121762..-28058
on x=-65152..-11147,y=22489..91432,z=-58782..1780
on x=-120100..-32970,y=-46592..27473,z=-11695..61039
on x=-18631..37533,y=-124565..-50804,z=-35667..28308
on x=-57817..18248,y=49321..117703,z=5745..55881
on x=14781..98692,y=-1341..70827,z=15753..70151
on x=-34419..55919,y=-19626..40991,z=39015..114138
on x=-60785..11593,y=-56135..2999,z=-95368..-26915
on x=-32178..58085,y=17647..101866,z=-91405..-8878
on x=-53655..12091,y=50097..105568,z=-75335..-4862
on x=-111166..-40997,y=-71714..2688,z=5609..50954
on x=-16602..70118,y=-98693..-44401,z=5197..76897
on x=16383..101554,y=4615..83635,z=-44907..18747
off x=-95822..-15171,y=-19987..48940,z=10804..104439
on x=-89813..-14614,y=16069..88491,z=-3297..45228
on x=41075..99376,y=-20427..49978,z=-52012..13762
on x=-21330..50085,y=-17944..62733,z=-112280..-30197
on x=-16478..35915,y=36008..118594,z=-7885..47086
off x=-98156..-27851,y=-49952..43171,z=-99005..-8456
off x=2032..69770,y=-71013..4824,z=7471..94418
on x=43670..120875,y=-42068..12382,z=-24787..38892
off x=37514..111226,y=-45862..25743,z=-16714..54663
off x=25699..97951,y=-30668..59918,z=-15349..69697
off x=-44271..17935,y=-9516..60759,z=49131..112598
on x=-61695..-5813,y=40978..94975,z=8655..80240
off x=-101086..-9439,y=-7088..67543,z=33935..83858
off x=18020..114017,y=-48931..32606,z=21474..89843
off x=-77139..10506,y=-89994..-18797,z=-80..59318
off x=8476..79288,y=-75520..11602,z=-96624..-24783
on x=-47488..-1262,y=24338..100707,z=16292..72967
off x=-84341..13987,y=2429..92914,z=-90671..-1318
off x=-37810..49457,y=-71013..-7894,z=-105357..-13188
off x=-27365..46395,y=31009..98017,z=15428..76570
off x=-70369..-16548,y=22648..78696,z=-1892..86821
on x=-53470..21291,y=-120233..-33476,z=-44150..38147
off x=-93533..-4276,y=-16170..68771,z=-104985..-24507
";

    #[test]
    fn p2_example() {
        assert_eq!(part2(EXAMPLE3_INPUT), 2758514936282235);
    }

    #[test]
    fn p2_input() {
        let input = std::fs::read_to_string("input/day22").expect("reading input file");
        assert_eq!(part2(&input), 1325473814582641);
    }
}
