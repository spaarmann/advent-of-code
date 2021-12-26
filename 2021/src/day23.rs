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
struct State<const ROOM_LEN: usize> {
    //#############
    //#...........# <- hallway, 0-10, from left to right
    //###D#D#C#B### <- index 0 in room
    //  #B#A#A#C#   <- index 1 in room
    //  #########
    //   0 1 2 3    <- room indices
    hallway: [Option<Amphipod>; 11],
    rooms: [[Option<Amphipod>; ROOM_LEN]; 4],
}

impl<const ROOM_LEN: usize> State<ROOM_LEN> {
    fn is_finish_state(&self) -> bool {
        self.hallway.iter().all(|s| s.is_none())
            && self.rooms[0].iter().all(|&s| s == Some(Amphipod::Amber))
            && self.rooms[1].iter().all(|&s| s == Some(Amphipod::Bronze))
            && self.rooms[2].iter().all(|&s| s == Some(Amphipod::Copper))
            && self.rooms[3].iter().all(|&s| s == Some(Amphipod::Desert))
    }

    fn possible_next_states(&self) -> Vec<(Self, u64)> {
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
            let Some(room_origin) = room.iter().position(|s| s.is_some()) else { continue; };

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
    // should go into spot 0 in the room, and 1 if it can and should go into spot 1 in the room,
    // etc.
    fn get_room_target(&self, amphi: Amphipod, room: usize) -> Option<usize> {
        if room != amphi.room_idx() {
            return None;
        }

        let first_occupied = self.rooms[room].iter().position(|s| s.is_some());
        if let Some(first_occupied) = first_occupied {
            if first_occupied == 0 {
                return None;
            }

            // Check that all the amphipods from the first occupied to the end are filled with
            // the correct amphipods.
            if self.rooms[room][first_occupied..ROOM_LEN]
                .iter()
                .all(|&s| s == Some(amphi))
            {
                return Some(first_occupied - 1);
            } else {
                return None;
            }
        } else {
            // All empty, take the furthes spot back.
            return Some(ROOM_LEN - 1);
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

impl<const ROOM_LEN: usize> Debug for State<ROOM_LEN> {
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
        f.write_str("###\n")?;

        for i in 1..ROOM_LEN {
            f.write_str("  #")?;
            f.write_str(
                &self
                    .rooms
                    .iter()
                    .map(|r| r[i])
                    .map(|s| s.map(|amphi| amphi.to_char()).unwrap_or('.'))
                    .intersperse('#')
                    .collect::<String>(),
            )?;
            f.write_str("#  \n")?;
        }
        f.write_str("  #########  \n")
    }
}

fn parse<const ROOM_LEN: usize>(lines: Vec<&str>) -> State<ROOM_LEN> {
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

    let mut rooms = [[None; ROOM_LEN]; 4];
    for i in 0..ROOM_LEN {
        rooms[0][i] = parse_amphi(lines[2 + i].as_bytes()[3]);
        rooms[1][i] = parse_amphi(lines[2 + i].as_bytes()[5]);
        rooms[2][i] = parse_amphi(lines[2 + i].as_bytes()[7]);
        rooms[3][i] = parse_amphi(lines[2 + i].as_bytes()[9]);
    }

    State { hallway, rooms }
}

// Implementation of Dijkstra's algorithm adapted from
// https://doc.rust-lang.org/std/collections/binary_heap/index.html.

#[derive(Debug, Clone, Eq, PartialEq)]
struct Node<const ROOM_LEN: usize> {
    cost: u64,
    state: State<ROOM_LEN>,
}

// The priority queue depends on `Ord`.
// Explicitly implement the trait so the queue becomes a min-heap
// instead of a max-heap.
impl<const ROOM_LEN: usize> Ord for Node<ROOM_LEN> {
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

impl<const ROOM_LEN: usize> PartialOrd for Node<ROOM_LEN> {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

fn dijkstra<const ROOM_LEN: usize>(start: State<ROOM_LEN>) -> u64 {
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
    dijkstra(parse::<2>(input.lines().collect()))
}

pub fn part2(input: &str) -> u64 {
    let mut lines = input.lines().collect::<Vec<_>>();
    lines.insert(3, "  #D#C#B#A#");
    lines.insert(4, "  #D#B#A#C#");
    dijkstra(parse::<4>(lines))
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
        assert_eq!(part2(EXAMPLE_INPUT), 44169);
    }

    #[test]
    fn p2_input() {
        let input = std::fs::read_to_string("input/day23").expect("reading input file");
        assert_eq!(part2(&input), 43481);
    }
}
