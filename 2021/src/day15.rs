use std::cmp::Ordering;
use std::collections::BinaryHeap;

struct Map {
    size: usize,
    entry_costs: Vec<usize>,
}

impl Map {
    fn parse(input: &str) -> Self {
        let size = input.lines().next().unwrap().len();
        let entry_costs = input
            .lines()
            .flat_map(|l| l.chars().map(|c| c.to_digit(10).unwrap() as usize))
            .collect();
        Self { size, entry_costs }
    }

    fn entry_cost(&self, (x, y): (usize, usize)) -> usize {
        self.entry_costs[y * self.size + x]
    }

    fn idx(&self, (x, y): (usize, usize)) -> usize {
        y * self.size + x
    }

    fn neighbors(&self, (x, y): (usize, usize)) -> impl Iterator<Item = (usize, usize)> + '_ {
        [(-1, 0), (1, 0), (0, -1), (0, 1)]
            .into_iter()
            .filter_map(move |offset| {
                let x = x as isize + offset.0;
                let y = y as isize + offset.1;
                if x < 0 || x >= self.size as isize || y < 0 || y >= self.size as isize {
                    None
                } else {
                    Some((x as usize, y as usize))
                }
            })
    }
}

// Implementation of Dijkstra's algorithm adapted from
// https://doc.rust-lang.org/std/collections/binary_heap/index.html.

#[derive(Copy, Clone, Eq, PartialEq)]
struct State {
    cost: usize,
    position: (usize, usize),
}

// The priority queue depends on `Ord`.
// Explicitly implement the trait so the queue becomes a min-heap
// instead of a max-heap.
impl Ord for State {
    fn cmp(&self, other: &Self) -> Ordering {
        // Notice that the we flip the ordering on costs.
        // In case of a tie we compare positions - this step is necessary
        // to make implementations of `PartialEq` and `Ord` consistent.
        other
            .cost
            .cmp(&self.cost)
            .then_with(|| self.position.cmp(&other.position))
    }
}

impl PartialOrd for State {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

fn dijkstra(map: &Map, start: (usize, usize), goal: (usize, usize)) -> usize {
    let mut dist = vec![usize::MAX; map.size * map.size];
    let mut heap = BinaryHeap::new();

    dist[map.idx(start)] = 0;
    heap.push(State {
        cost: 0,
        position: start,
    });

    while let Some(State { cost, position }) = heap.pop() {
        if position == goal {
            return cost;
        }

        if cost > dist[map.idx(position)] {
            continue;
        }

        // For each node we can reach, see if we can find a way with
        // a lower cost going through this node
        for neighbor in map.neighbors(position) {
            let next = State {
                cost: cost + map.entry_cost(neighbor),
                position: neighbor,
            };

            if next.cost < dist[map.idx(next.position)] {
                heap.push(next);
                dist[map.idx(next.position)] = next.cost;
            }
        }
    }

    panic!("could not find path!")
}

pub fn part1(input: &str) -> u64 {
    let map = Map::parse(input);
    dijkstra(&map, (0, 0), (map.size - 1, map.size - 1)) as u64
}

pub fn part2(input: &str) -> u64 {
    let partial_map = Map::parse(input);

    let full_size = partial_map.size * 5;
    let mut full_costs = vec![0; full_size * full_size];
    for map_y in 0..5 {
        for map_x in 0..5 {
            for y in 0..partial_map.size {
                for x in 0..partial_map.size {
                    let orig_cost = partial_map.entry_cost((x, y));
                    let mut new_cost = orig_cost + map_y + map_x;
                    while new_cost > 9 {
                        new_cost -= 9;
                    }
                    full_costs[(map_y * partial_map.size + y) * full_size
                        + map_x * partial_map.size
                        + x] = new_cost;
                }
            }
        }
    }

    let full_map = Map {
        size: full_size,
        entry_costs: full_costs,
    };
    dijkstra(&full_map, (0, 0), (full_map.size - 1, full_map.size - 1)) as u64
}

#[cfg(test)]
mod tests {
    use super::*;

    const EXAMPLE_INPUT: &'static str = "1163751742
1381373672
2136511328
3694931569
7463417111
1319128137
1359912421
3125421639
1293138521
2311944581";

    #[test]
    fn p1_example() {
        assert_eq!(part1(EXAMPLE_INPUT), 40);
    }

    #[test]
    fn p1_input() {
        let input = std::fs::read_to_string("input/day15").expect("reading input file");
        assert_eq!(part1(&input), 398);
    }

    #[test]
    fn p2_example() {
        assert_eq!(part2(EXAMPLE_INPUT), 315);
    }

    #[test]
    fn p2_input() {
        let input = std::fs::read_to_string("input/day15").expect("reading input file");
        assert_eq!(part2(&input), 2817);
    }
}
