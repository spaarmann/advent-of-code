use std::{
    cmp::Ordering,
    collections::{BinaryHeap, HashMap},
    fmt::Debug,
};

#[derive(Debug, Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
enum Amphipod {
    Amber,
    Bronze,
    Copper,
    Desert,
}

impl Amphipod {
    fn from_byte(c: u8) -> Amphipod {
        match c {
            b'A' => Amphipod::Amber,
            b'B' => Amphipod::Bronze,
            b'C' => Amphipod::Copper,
            b'D' => Amphipod::Desert,
            _ => panic!(),
        }
    }

    fn to_char(self) -> char {
        match self {
            Amphipod::Amber => 'A',
            Amphipod::Bronze => 'B',
            Amphipod::Copper => 'C',
            Amphipod::Desert => 'D',
        }
    }

    fn room_idx(self) -> usize {
        match self {
            Amphipod::Amber => 0,
            Amphipod::Bronze => 1,
            Amphipod::Copper => 2,
            Amphipod::Desert => 3,
        }
    }

    fn room_entry_idx(self) -> usize {
        match self {
            Amphipod::Amber => 2,
            Amphipod::Bronze => 4,
            Amphipod::Copper => 6,
            Amphipod::Desert => 8,
        }
    }

    fn energy_cost(self) -> u64 {
        match self {
            Amphipod::Amber => 1,
            Amphipod::Bronze => 10,
            Amphipod::Copper => 100,
            Amphipod::Desert => 1000,
        }
    }
}

#[derive(Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
struct State {
    //#############
    //#...........# <- hallway, 0-10, from left to right
    //###D#D#C#B### <- index 0 in room
    //  #B#A#A#C#   <- index 1 in room
    //  #########
    //   0 1 2 3    <- room indices
    hallway: [Option<Amphipod>; 11],
    rooms: [[Option<Amphipod>; 2]; 4],
}

impl State {
    fn is_finish_state(&self) -> bool {
        self.hallway.iter().all(|s| s.is_none())
            && self.rooms[0].iter().all(|&s| s == Some(Amphipod::Amber))
            && self.rooms[1].iter().all(|&s| s == Some(Amphipod::Bronze))
            && self.rooms[2].iter().all(|&s| s == Some(Amphipod::Copper))
            && self.rooms[3].iter().all(|&s| s == Some(Amphipod::Desert))
    }

    fn possible_next_states(&self) -> Vec<(State, u64)> {
        let mut states = Vec::new();

        // Any amphipod already in the hallway will only move into its own room, and only if there
        // isn't an amphipod of another type in there already (and only if the way there is clear).
        for (idx, spot) in self.hallway.iter().enumerate() {
            if let Some(amphi) = *spot {
                let room_idx = amphi.room_idx();
                let room_entry_idx = amphi.room_entry_idx();
                if let Some(idx_in_room) = self.get_room_target(amphi, room_idx) {
                    if self.hallway_free(idx, room_entry_idx) {
                        let mut new_hallway = self.hallway.clone();
                        new_hallway[idx] = None;

                        let mut new_rooms = self.rooms.clone();
                        new_rooms[room_idx][idx_in_room] = Some(amphi);

                        let steps = idx.abs_diff(room_entry_idx) + idx_in_room + 1;

                        states.push((
                            State {
                                hallway: new_hallway,
                                rooms: new_rooms,
                            },
                            steps as u64 * amphi.energy_cost(),
                        ));
                    }
                }
            }
        }

        // An amphipod in a room can move into any free, reachable hallway spot, that isn't a room
        // entry spot. (They could also directly move into their target room, but we can model that
        // as two steps, stopping in the hallway.)
        for (room_idx, room) in self.rooms.iter().enumerate() {
            let room_origin = match room {
                [Some(_), _] => 0,
                [None, Some(_)] => 1,
                _ => continue,
            };

            let amphi = room[room_origin].unwrap();
            let hallway_start = Self::room_to_room_entry(room_idx);

            for hallway_target in 0..11 {
                if Self::is_room_entry_spot(hallway_target) {
                    continue;
                }

                if self.hallway_free(hallway_start, hallway_target) {
                    let mut new_hallway = self.hallway.clone();
                    new_hallway[hallway_target] = Some(amphi);

                    let mut new_rooms = self.rooms.clone();
                    new_rooms[room_idx][room_origin] = None;

                    let steps = hallway_start.abs_diff(hallway_target) + room_origin + 1;

                    states.push((
                        State {
                            hallway: new_hallway,
                            rooms: new_rooms,
                        },
                        steps as u64 * amphi.energy_cost(),
                    ));
                }
            }
        }

        states
    }

    // Whether it is possible to go from 'start' to 'end' (inlusive) in the hallway without
    // crossing someone else.
    fn hallway_free(&self, start: usize, end: usize) -> bool {
        if start == end {
            return true;
        }

        let (min, max) = if start < end {
            (start + 1, end)
        } else {
            (end, start - 1)
        };

        self.hallway[min..=max].iter().all(|s| s.is_none())
    }

    // Returns None if the given amphipod cannot currently enter the given room, 0 if it can and
    // should go into spot 0 in the room, and 1 if it can and should go into spot 1 in the room.
    fn get_room_target(&self, amphi: Amphipod, room: usize) -> Option<usize> {
        if room != amphi.room_idx() {
            return None;
        }

        match self.rooms[room] {
            [None, None] => Some(1),
            [None, Some(a)] if amphi == a => Some(0),
            _ => None,
        }
    }

    fn is_room_entry_spot(room: usize) -> bool {
        room == 2 || room == 4 || room == 6 || room == 8
    }

    fn room_to_room_entry(room: usize) -> usize {
        match room {
            0 => 2,
            1 => 4,
            2 => 6,
            3 => 8,
            _ => panic!(),
        }
    }
}

impl Debug for State {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str("#############\n#")?;
        f.write_str(
            &self
                .hallway
                .iter()
                .map(|s| s.map(|amphi| amphi.to_char()).unwrap_or('.'))
                .collect::<String>(),
        )?;
        f.write_str("#\n###")?;
        f.write_str(
            &self
                .rooms
                .iter()
                .map(|r| r[0])
                .map(|s| s.map(|amphi| amphi.to_char()).unwrap_or('.'))
                .intersperse('#')
                .collect::<String>(),
        )?;
        f.write_str("###\n  #")?;
        f.write_str(
            &self
                .rooms
                .iter()
                .map(|r| r[1])
                .map(|s| s.map(|amphi| amphi.to_char()).unwrap_or('.'))
                .intersperse('#')
                .collect::<String>(),
        )?;
        f.write_str("#  \n  #########  \n")
    }
}

fn parse(input: &str) -> State {
    let lines = input.lines().collect::<Vec<_>>();

    let parse_amphi = |b| {
        if b == b'.' {
            None
        } else {
            Some(Amphipod::from_byte(b))
        }
    };

    let mut hallway = [None; 11];
    for i in 0..11 {
        hallway[i] = parse_amphi(lines[1].as_bytes()[i + 1]);
    }

    let mut rooms = [[None; 2]; 4];
    rooms[0][0] = parse_amphi(lines[2].as_bytes()[3]);
    rooms[0][1] = parse_amphi(lines[3].as_bytes()[3]);
    rooms[1][0] = parse_amphi(lines[2].as_bytes()[5]);
    rooms[1][1] = parse_amphi(lines[3].as_bytes()[5]);
    rooms[2][0] = parse_amphi(lines[2].as_bytes()[7]);
    rooms[2][1] = parse_amphi(lines[3].as_bytes()[7]);
    rooms[3][0] = parse_amphi(lines[2].as_bytes()[9]);
    rooms[3][1] = parse_amphi(lines[3].as_bytes()[9]);

    State { hallway, rooms }
}

// Implementation of Dijkstra's algorithm adapted from
// https://doc.rust-lang.org/std/collections/binary_heap/index.html.

#[derive(Debug, Clone, Eq, PartialEq)]
struct Node {
    cost: u64,
    state: State,
}

// The priority queue depends on `Ord`.
// Explicitly implement the trait so the queue becomes a min-heap
// instead of a max-heap.
impl Ord for Node {
    fn cmp(&self, other: &Self) -> Ordering {
        // Notice that the we flip the ordering on costs.
        // In case of a tie we compare positions - this step is necessary
        // to make implementations of `PartialEq` and `Ord` consistent.
        other
            .cost
            .cmp(&self.cost)
            .then_with(|| self.state.cmp(&other.state))
    }
}

impl PartialOrd for Node {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

fn dijkstra(start: State) -> u64 {
    let mut dist = HashMap::new();
    let mut heap = BinaryHeap::new();

    dist.insert(start.clone(), 0);
    heap.push(Node {
        cost: 0,
        state: start,
    });

    while let Some(Node { cost, state }) = heap.pop() {
        if state.is_finish_state() {
            return cost;
        }

        if cost > *dist.get(&state).unwrap_or(&u64::MAX) {
            continue;
        }

        // For each node we can reach, see if we can find a way with a lower cost going through
        // this node.
        for neighbor in state.possible_next_states() {
            let next = Node {
                cost: cost + neighbor.1,
                state: neighbor.0,
            };

            if next.cost < *dist.get(&next.state).unwrap_or(&u64::MAX) {
                dist.insert(next.state.clone(), next.cost);
                heap.push(next);
            }
        }
    }

    panic!("could not find path!")
}

pub fn part1(input: &str) -> u64 {
    dijkstra(parse(input))
}

pub fn part2(input: &str) -> u64 {
    todo!()
}

#[cfg(test)]
mod tests {
    use super::*;

    const EXAMPLE_INPUT: &'static str = "#############
#...........#
###B#C#B#D###
  #A#D#C#A#
  #########
";

    #[test]
    fn p1_example() {
        assert_eq!(part1(EXAMPLE_INPUT), 12521);
    }

    #[test]
    fn p1_input() {
        let input = std::fs::read_to_string("input/day23").expect("reading input file");
        assert_eq!(part1(&input), 16157);
    }

    #[test]
    fn p2_example() {
        assert_eq!(part2(EXAMPLE_INPUT), todo!());
    }

    #[test]
    fn p2_input() {
        let input = std::fs::read_to_string("input/day23").expect("reading input file");
        assert_eq!(part2(&input), todo!());
    }
}
