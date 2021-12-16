use bitvec::prelude::*;

type BVec = BitVec<Msb0, usize>;
type BSlice = BitSlice<Msb0, usize>;

fn parse_string_to_bits(input: &str) -> BVec {
    let bit_count = input.len() * 4;
    let mut bits = BitVec::new();
    bits.resize(bit_count, false);

    for (i, c) in input.chars().enumerate() {
        let num = c.to_digit(16).unwrap() as u8;
        bits[(i * 4)..((i + 1) * 4)].store(num);
    }

    bits
}

#[derive(Debug)]
struct Packet {
    version: u8,
    payload: PacketPayload,
}

#[derive(Debug)]
enum PacketPayload {
    Literal(usize),
    Operator(Operator, Vec<Packet>),
}

#[derive(Debug, Copy, Clone)]
enum Operator {
    Sum,
    Product,
    Min,
    Max,
    Greater,
    Less,
    Equal,
}

impl Operator {
    fn from_id(id: u8) -> Operator {
        match id {
            0 => Operator::Sum,
            1 => Operator::Product,
            2 => Operator::Min,
            3 => Operator::Max,
            5 => Operator::Greater,
            6 => Operator::Less,
            7 => Operator::Equal,
            _ => panic!("invalid operator id"),
        }
    }
}

fn parse_literal(bits: &BSlice) -> (usize, usize) {
    let mut num = 0;
    let mut bits_read = 0;

    for chunk in bits.chunks(5) {
        bits_read += 5;

        num = num << 4;
        num |= chunk[1..].load_be::<usize>();

        if !chunk[0] {
            break;
        }
    }

    (bits_read, num)
}

fn parse_packets_count(bits: &BSlice, packet_count: usize) -> (usize, Vec<Packet>) {
    let mut packets = Vec::with_capacity(packet_count);
    let mut total_read = 0;

    for _ in 0..packet_count {
        let (read, packet) = parse_packet(&bits[total_read..]);
        total_read += read;
        packets.push(packet);
    }

    (total_read, packets)
}

fn parse_packets_num_bits(bits: &BSlice, bits_to_read: usize) -> (usize, Vec<Packet>) {
    let mut packets = Vec::new();
    let mut total_read = 0;

    while total_read < bits_to_read {
        let (read, packet) = parse_packet(&bits[total_read..]);
        total_read += read;
        packets.push(packet);
    }

    (total_read, packets)
}

fn parse_packet(bits: &BSlice) -> (usize, Packet) {
    let version = bits[0..3].load_be::<u8>();
    let type_id = bits[3..6].load_be::<u8>();

    let payload_bits = &bits[6..];
    let (payload_bits_read, payload) = match type_id {
        4 => {
            let (read, num) = parse_literal(payload_bits);
            (read, PacketPayload::Literal(num))
        }
        operator_id => {
            let op = Operator::from_id(operator_id);
            let length_type_id = payload_bits[0];
            if length_type_id {
                let packet_count = payload_bits[1..12].load_be::<usize>();
                let (read, packets) = parse_packets_count(&payload_bits[12..], packet_count);
                (read + 12, PacketPayload::Operator(op, packets))
            } else {
                let to_read = payload_bits[1..16].load_be::<usize>();
                let (read, packets) = parse_packets_num_bits(&payload_bits[16..], to_read);
                assert_eq!(read, to_read);
                (read + 16, PacketPayload::Operator(op, packets))
            }
        }
    };

    (6 + payload_bits_read, Packet { version, payload })
}

fn walk_packets<F: FnMut(&Packet)>(packet: Packet, f: &mut F) {
    f(&packet);
    if let PacketPayload::Operator(_, packets) = packet.payload {
        for p in packets {
            walk_packets(p, f);
        }
    }
}

pub fn part1(input: &str) -> u64 {
    let bits = parse_string_to_bits(input.trim());
    let (_, packet) = parse_packet(&bits);

    let mut version_sum = 0;
    walk_packets(packet, &mut |p: &Packet| version_sum += p.version as u64);

    version_sum
}

fn evaluate_packet(packet: Packet) -> u64 {
    match packet.payload {
        PacketPayload::Literal(n) => n as u64,
        PacketPayload::Operator(op, children) => {
            let mut evaluated_children = children.into_iter().map(evaluate_packet);
            match op {
                Operator::Sum => evaluated_children.sum(),
                Operator::Product => evaluated_children.product(),
                Operator::Min => evaluated_children.min().unwrap(),
                Operator::Max => evaluated_children.max().unwrap(),
                Operator::Greater => {
                    (evaluated_children.next().unwrap() > evaluated_children.next().unwrap()) as u64
                }
                Operator::Less => {
                    (evaluated_children.next().unwrap() < evaluated_children.next().unwrap()) as u64
                }
                Operator::Equal => {
                    (evaluated_children.next().unwrap() == evaluated_children.next().unwrap())
                        as u64
                }
            }
        }
    }
}

pub fn part2(input: &str) -> u64 {
    let bits = parse_string_to_bits(input.trim());
    let (_, packet) = parse_packet(&bits);

    evaluate_packet(packet)
}

#[cfg(test)]
mod tests {
    use super::*;

    const EXAMPLE1_INPUT: &'static str = "8A004A801A8002F478";
    const EXAMPLE2_INPUT: &'static str = "620080001611562C8802118E34";
    const EXAMPLE3_INPUT: &'static str = "C0015000016115A2E0802F182340";
    const EXAMPLE4_INPUT: &'static str = "A0016C880162017C3686B18A3D4780";

    #[test]
    fn p1_example1() {
        assert_eq!(part1(EXAMPLE1_INPUT), 16);
    }

    #[test]
    fn p1_example2() {
        assert_eq!(part1(EXAMPLE2_INPUT), 12);
    }

    #[test]
    fn p1_example3() {
        assert_eq!(part1(EXAMPLE3_INPUT), 23);
    }
    #[test]
    fn p1_example4() {
        assert_eq!(part1(EXAMPLE4_INPUT), 31);
    }

    #[test]
    fn p1_input() {
        let input = std::fs::read_to_string("input/day16").expect("reading input file");
        assert_eq!(part1(&input), 901);
    }

    #[test]
    fn p2_example() {
        assert_eq!(part2("C200B40A82"), 3);
        assert_eq!(part2("04005AC33890"), 54);
        assert_eq!(part2("880086C3E88112"), 7);
        assert_eq!(part2("CE00C43D881120"), 9);
        assert_eq!(part2("D8005AC2A8F0"), 1);
        assert_eq!(part2("F600BC2D8F"), 0);
        assert_eq!(part2("9C005AC2F8F0"), 0);
        assert_eq!(part2("9C0141080250320F1802104A08"), 1);
    }

    #[test]
    fn p2_input() {
        let input = std::fs::read_to_string("input/day16").expect("reading input file");
        assert_eq!(part2(&input), 110434737925);
    }
}
