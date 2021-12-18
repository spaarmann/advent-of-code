#[derive(Debug)]
struct Area {
    x_min: i64,
    x_max: i64,
    y_min: i64,
    y_max: i64,
}

impl Area {
    fn parse(input: &str) -> Self {
        let mut it = input
            .split(", ")
            .map(|s| s.split_once('=').unwrap().1)
            .map(|s| s.split_once("..").unwrap());

        let x = it.next().unwrap();
        let x_min = x.0.parse().unwrap();
        let x_max = x.1.parse().unwrap();

        let y = it.next().unwrap();
        let y_min = y.0.parse().unwrap();
        let y_max = y.1.parse().unwrap();

        Self {
            x_min,
            x_max,
            y_min,
            y_max,
        }
    }

    fn contains(&self, x: i64, y: i64) -> bool {
        self.x_min <= x && self.x_max >= x && self.y_min <= y && self.y_max >= y
    }
}

fn simulate_path(vx_init: i64, vy_init: i64, target: &Area) -> Option<i64> {
    let mut vx = vx_init;
    let mut vy = vy_init;
    let mut max_y = 0;

    let mut x = 0;
    let mut y = 0;

    loop {
        x += vx;
        y += vy;
        max_y = max_y.max(y);

        vx = (vx - 1).max(0);
        vy -= 1;

        if target.contains(x, y) {
            return Some(max_y);
        }
        if x > target.x_max || y < target.y_min {
            return None;
        }
    }
}

pub fn run(input: &str) -> (u64, u64) {
    let target = Area::parse(input.trim().split_once(": ").unwrap().1);

    // An initial velocity of 'vx' leads to x(n) = vx + (vx - 1) + (vx - 2) + (vx - 3) + ... + (vx - (n-1))
    // except with each term being capped to not get negative.
    // So it'll just be zero after n=vx+1 steps, so n=vx is the last relevant steps.
    // So we get
    //   max_x_distance(vx) = vx + (vx - 1) + (vx - 2) + ... + 2 + 1
    //                      = 1 + 2 + 3 + 4 + ... + vx
    //                      = (vx * (vx - 1)) / 2
    // We want to get at least to x=x_min, so...
    //   x_min = max_x_distance(vx)
    //   x_min = (vx * (vx - 1)) / 2
    //   2 * x_min = vx * (vx - 1)
    //   2 * x_min = vx^2 - vx
    //   0 = vx^2 - vx - 2 * x_min
    //   vx = 0.5 * (1 +- sqrt(8 * x_min + 1))
    let min_vx = (0.5 * (1.0 + (8.0 * target.x_min as f32 + 1.0).sqrt())).floor() as i64;

    // There's probably a better upper bound one could find, but... this is nice and simple.
    let max_vx = target.x_max;

    let mut max_y_reached = 0;
    let mut hit_count = 0;
    for vx_init in min_vx..=max_vx {
        // Find minimum number of steps needed to reach the target area.
        //   x(n) = vx + (vx - 1) + (vx - 2) + (vx - 3) + ... + (vx - (n-1))
        // You could probably calculate this, but, again, this is easy:
        //let mut min_n = 0;
        //loop {
        //    let x: i64 = (0..=(min_n - 1).max(0)).map(|n| vx_init - n).sum();
        //    if x >= target.x_min {
        //        break;
        //    }
        //    min_n += 1;
        //}

        //let mut max_n = None;
        //let mut current_n = 0;
        //let mut last_x = -1;
        //loop {
        //    let x: i64 = (0..=(current_n - 1).max(0)).map(|n| vx_init - n).sum();
        //    if last_x == x {
        //        // Looks like we reached vx=0 within the target area.
        //        max_n = None;
        //        break;
        //    }
        //    if x > target.x_max {
        //        break;
        //    }
        //    max_n = Some(current_n);
        //    current_n += 1;
        //    last_x = x;
        //}

        // Okay, so we can do between min_n and max_n.unwrap_or(inf) steps and reach the target
        // area on x.

        // In principle, we could now start doing a similar dance with y, but let's just try this
        // for now:
        for vy_init in -1000..1000 {
            if let Some(max_y) = simulate_path(vx_init, vy_init, &target) {
                max_y_reached = max_y_reached.max(max_y);
                hit_count += 1;
            }
        }
    }

    (max_y_reached as u64, hit_count)
}

pub fn part1(input: &str) -> u64 {
    run(input).0
}

pub fn part2(input: &str) -> u64 {
    run(input).1
}

#[cfg(test)]
mod tests {
    use super::*;

    const EXAMPLE_INPUT: &'static str = "target area: x=20..30, y=-10..-5";

    #[test]
    fn p1_example() {
        assert_eq!(part1(EXAMPLE_INPUT), 45);
    }

    #[test]
    fn p1_input() {
        let input = std::fs::read_to_string("input/day17").expect("reading input file");
        assert_eq!(part1(&input), 4560);
    }

    #[test]
    fn p2_example() {
        assert_eq!(part2(EXAMPLE_INPUT), 112);
    }

    #[test]
    fn p2_input() {
        let input = std::fs::read_to_string("input/day17").expect("reading input file");
        assert_eq!(part2(&input), 3344);
    }
}
