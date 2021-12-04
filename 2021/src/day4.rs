#[derive(Debug)]
struct BingoBoard {
    board: [(bool, u64); 5 * 5],
}

impl BingoBoard {
    fn from_lines(lines: &[&str]) -> BingoBoard {
        let mut board = [(false, 0); 5 * 5];
        for (i, line) in lines.iter().enumerate() {
            for (j, num) in line.split_whitespace().enumerate() {
                board[i * 5 + j] = (false, num.parse().unwrap());
            }
        }

        BingoBoard { board }
    }

    fn mark_number(&mut self, num: u64) {
        for entry in self.board.iter_mut() {
            if entry.1 == num {
                entry.0 = true;
            }
        }
    }

    fn has_won(&self) -> bool {
        for row in 0..5 {
            if (0..5).all(|col| self.board[row * 5 + col].0) {
                return true;
            }
        }

        for col in 0..5 {
            if (0..5).all(|row| self.board[row * 5 + col].0) {
                return true;
            }
        }

        false
    }

    fn score(&self) -> u64 {
        self.board
            .iter()
            .filter(|entry| !entry.0)
            .map(|entry| entry.1)
            .sum()
    }
}

fn parse(input: &str) -> (Vec<u64>, Vec<BingoBoard>) {
    let mut lines = input.lines();
    let number_line = lines.next().unwrap();
    let numbers = number_line.split(',').map(|n| n.parse().unwrap()).collect();
    lines.next().unwrap(); // Discard empty line

    let mut boards = Vec::new();
    let mut current_board_lines = Vec::new();

    for line in lines {
        if line.is_empty() {
            boards.push(BingoBoard::from_lines(&current_board_lines));
            current_board_lines.clear();
        } else {
            current_board_lines.push(line);
        }
    }
    boards.push(BingoBoard::from_lines(&current_board_lines));

    (numbers, boards)
}

pub fn part1(input: &str) -> u64 {
    let (nums, mut boards) = parse(input);

    for n in nums {
        for board in boards.iter_mut() {
            board.mark_number(n);

            if board.has_won() {
                return board.score() * n;
            }
        }
    }

    panic!("No board won!")
}

pub fn part2(input: &str) -> u64 {
    let (nums, mut boards) = parse(input);

    for n in nums {
        for board in boards.iter_mut() {
            board.mark_number(n);
        }

        if boards.len() == 1 && boards[0].has_won() {
            return boards[0].score() * n;
        }

        boards.retain(|b| !b.has_won());
    }

    panic!("No board won!")
}

#[cfg(test)]
mod tests {
    use super::*;

    const EXAMPLE_INPUT: &'static str =
        "7,4,9,5,11,17,23,2,0,14,21,24,10,16,13,6,15,25,12,22,18,20,8,19,3,26,1

22 13 17 11  0
 8  2 23  4 24
21  9 14 16  7
 6 10  3 18  5
 1 12 20 15 19

 3 15  0  2 22
 9 18 13 17  5
19  8  7 25 23
20 11 10 24  4
14 21 16 12  6

14 21 17 24  4
10 16 15  9 19
18  8 23 26 20
22 11 13  6  5
 2  0 12  3  7";

    #[test]
    fn p1_example() {
        assert_eq!(part1(EXAMPLE_INPUT), 4512);
    }

    #[test]
    fn p1_input() {
        let input = std::fs::read_to_string("input/day4").expect("reading input file");
        assert_eq!(part1(&input), 23177);
    }

    #[test]
    fn p2_example() {
        assert_eq!(part2(EXAMPLE_INPUT), 1924);
    }

    #[test]
    fn p2_input() {
        let input = std::fs::read_to_string("input/day4").expect("reading input file");
        assert_eq!(part2(&input), 6804);
    }
}
