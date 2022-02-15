use std::collections::HashMap;
use std::convert::TryInto;

const BOARD_SIZE: u8 = 15;
const WIN_COUNT: i32 = 5;
const fn board_cap() -> usize {
    (BOARD_SIZE * 2) as usize
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum Camp {
    Write,
    Black,
}

impl Camp {
    fn reverse(&self) -> Camp {
        match self {
            Camp::Write => Camp::Black,
            Camp::Black => Camp::Write,
        }
    }
}

pub struct Piece {
    camp: Camp,
}

impl Piece {
    pub fn black() -> Self {
        Piece { camp: Camp::Black }
    }
    pub fn write() -> Self {
        Piece { camp: Camp::Write }
    }
}

enum Direction {
    Top,
    TopRight,
    Right,
    BottomRight,
    Bottom,
    BottomLeft,
    Left,
    TopLeft,
}

impl Direction {
    fn reverse(&self) -> Direction {
        match self {
            Direction::Top => Direction::Bottom,
            Direction::TopRight => Direction::BottomLeft,
            Direction::Right => Direction::Left,
            Direction::BottomRight => Direction::TopLeft,
            Direction::Bottom => Direction::Top,
            Direction::BottomLeft => Direction::TopRight,
            Direction::Left => Direction::Right,
            Direction::TopLeft => Direction::BottomRight,
        }
    }

    fn offset(&self) -> (i16, i16) {
        match self {
            Direction::Top => (0, -1),
            Direction::TopRight => (1, -1),
            Direction::Right => (1, 0),
            Direction::BottomRight => (1, 1),
            Direction::Bottom => (0, 1),
            Direction::BottomLeft => (-1, 1),
            Direction::Left => (-1, 0),
            Direction::TopLeft => (-1, -1),
        }
    }
}

#[derive(PartialEq, Eq, Debug, Hash, Clone, Copy)]
pub struct Position {
    x: u8,
    y: u8,
}
impl Position {
    pub fn new(x: u8, y: u8) -> Self {
        Position { x, y }
    }
}

impl Position {
    fn is_invalid_position(&self) -> bool {
        self.x > BOARD_SIZE - 1 || self.y > BOARD_SIZE - 1
    }

    fn near_direction(&self, direction: &Direction) -> Option<Position> {
        let offset = direction.offset();
        let x = (self.x as i16) + offset.0;
        let y = (self.y as i16) + offset.1;
        if x < 0 || x > (u8::MAX as i16) {
            None
        } else if y < 0 || y > (u8::MAX as i16) {
            None
        } else {
            Some(Position {
                x: x.try_into().unwrap(),
                y: y.try_into().unwrap(),
            })
        }
    }
}

pub struct Board {
    positions: HashMap<Position, Piece>,
    round: Camp,
    winner: Option<Camp>,
}

impl Default for Board {
    fn default() -> Self {
        Board {
            positions: HashMap::new(),
            round: Camp::Black,
            winner: None,
        }
    }
}

#[derive(Debug, PartialEq)]
pub enum Error {
    InvalidPosition,
    AlreadyInBoard,
    NotYourRound,
    GameIsOver,
    WrongBoard,
}

impl Board {
    fn get_piece(&self, position: &Position) -> Option<&Piece> {
        self.positions.get(position)
    }

    fn add_piece(&mut self, piece: Piece, position: Position) -> Result<(), Error> {
        if self.winner.is_some() {
            Err(Error::GameIsOver)
        } else if self.round != piece.camp {
            Err(Error::NotYourRound)
        } else if position.is_invalid_position() {
            Err(Error::InvalidPosition)
        } else if self.positions.contains_key(&position) {
            Err(Error::AlreadyInBoard)
        } else {
            self.positions.insert(position, piece);
            self.round = self.round.reverse();
            Ok(())
        }
    }

    fn win(&self, position: &Position) -> Option<Camp> {
        let piece = if let Some(piece) = self.get_piece(position) {
            piece
        } else {
            return None;
        };
        let directions = vec![
            Direction::Top,
            Direction::TopRight,
            Direction::Right,
            Direction::BottomRight,
        ];
        for d in &directions {
            let mut count = 1;
            self.direction_walk(position, piece, d, &mut count);
            let reverse_direction = d.reverse();
            self.direction_walk(position, piece, &reverse_direction, &mut count);
            if count >= WIN_COUNT {
                return Some(piece.camp.clone());
            }
        }
        None
    }

    fn direction_walk(
        &self,
        position: &Position,
        piece: &Piece,
        direction: &Direction,
        count: &mut i32,
    ) {
        let mut pos = position.clone();
        while let Some(new_pos) = pos.near_direction(direction) {
            if new_pos.is_invalid_position() {
                break;
            }
            if let Some(p) = self.get_piece(&new_pos) {
                if p.camp != piece.camp {
                    break;
                } else {
                    *count += 1;
                }
            } else {
                break;
            }
            pos = new_pos;
        }
    }

    pub fn next_round(&mut self, piece: Piece, position: Position) -> Result<Option<Camp>, Error> {
        let _ = self.add_piece(piece, position.clone())?;
        let result = self.win(&position);
        if result.is_some() {
            self.winner = result.clone();
        }
        Ok(result)
    }

    fn board(&self) -> [u16; board_cap()] {
        // 末尾 0 -> Black, 1 -> Write
        let mut board = [
            0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
            1,
        ];
        for (pos, piece) in &self.positions {
            let index_offset: usize = if piece.camp == Camp::Black {
                0
            } else {
                BOARD_SIZE as usize
            };
            let index = (pos.y as usize) + index_offset;
            board[index] = 1 << (pos.x + 1) | board[index]
        }
        board
    }

    pub fn bad_ending(board: &[u16; board_cap()]) -> Result<Self, Error> {
        let mut blacks = Vec::with_capacity(BOARD_SIZE as usize);
        let mut whites = Vec::with_capacity(BOARD_SIZE as usize);
        for (y, v) in board.iter().enumerate() {
            let mut n: u16 = 1;
            let is_black = v & 1 == 0;
            for x in 0u8..BOARD_SIZE {
                n = n << 1;
                if n & v > 0 {
                    let y_offset: u8 = if is_black { 0 } else { BOARD_SIZE };
                    let piece = if is_black {
                        Piece::black()
                    } else {
                        Piece::write()
                    };
                    if is_black {
                        blacks.push((Position::new(x, y as u8 - y_offset), piece));
                    } else {
                        whites.push((Position::new(x, y as u8 - y_offset), piece));
                    }
                }
            }
        }
        match blacks.len() - whites.len() {
            0..=1 => (),
            _ => return Err(Error::WrongBoard),
        }

        let mut board = Board::default();
        for _ in 0..blacks.len() {
            if let Some((position, piece)) = blacks.pop() {
                board.next_round(piece, position)?;
            }
            if let Some((position, piece)) = whites.pop() {
                board.next_round(piece, position)?;
            }
        }
        Ok(board)
    }
}

#[cfg(test)]
mod tests {
    use crate::{Board, Camp, Direction, Error, Piece, Position, board_cap};

    #[test]
    fn offset() {
        // 0,0  1,0  2,0
        // 0,1  1,1  2,1
        // 0,2  1,2  2,2
        let pos = Position::new(1, 1);
        assert_eq!(
            pos.near_direction(&Direction::Top),
            Some(Position::new(1, 0))
        );
        assert_eq!(
            pos.near_direction(&Direction::Bottom),
            Some(Position::new(1, 2))
        );
        assert_eq!(
            pos.near_direction(&Direction::Left),
            Some(Position::new(0, 1))
        );
        assert_eq!(
            pos.near_direction(&Direction::Right),
            Some(Position::new(2, 1))
        );
        assert_eq!(
            pos.near_direction(&Direction::TopLeft),
            Some(Position::new(0, 0))
        );
        assert_eq!(
            pos.near_direction(&Direction::TopRight),
            Some(Position::new(2, 0))
        );
        assert_eq!(
            pos.near_direction(&Direction::BottomLeft),
            Some(Position::new(0, 2))
        );
        assert_eq!(
            pos.near_direction(&Direction::BottomRight),
            Some(Position::new(2, 2))
        );
    }

    #[test]
    fn win() {
        let mut gobang = Board::default();
        assert!(gobang
            .add_piece(Piece::black(), Position::new(0, 1))
            .is_ok());
        assert!(gobang
            .add_piece(Piece::write(), Position::new(1, 1))
            .is_ok());
        assert!(gobang
            .add_piece(Piece::black(), Position::new(0, 2))
            .is_ok());
        assert!(gobang
            .add_piece(Piece::write(), Position::new(1, 2))
            .is_ok());
        assert!(gobang
            .add_piece(Piece::black(), Position::new(0, 3))
            .is_ok());
        assert!(gobang
            .add_piece(Piece::write(), Position::new(1, 3))
            .is_ok());
        assert!(gobang
            .add_piece(Piece::black(), Position::new(0, 4))
            .is_ok());
        assert!(gobang
            .add_piece(Piece::write(), Position::new(1, 4))
            .is_ok());
        assert!(gobang
            .add_piece(Piece::black(), Position::new(0, 5))
            .is_ok());
        assert_eq!(gobang.win(&Position::new(0, 5)), Some(Camp::Black));
    }

    #[test]
    fn next_round_win() {
        let mut gobang = Board::default();
        assert_eq!(
            gobang.next_round(Piece::black(), Position::new(0, 0)),
            Ok(None)
        );
        assert_eq!(
            gobang.next_round(Piece::write(), Position::new(0, 1)),
            Ok(None)
        );
        assert_eq!(
            gobang.next_round(Piece::black(), Position::new(1, 1)),
            Ok(None)
        );
        assert_eq!(
            gobang.next_round(Piece::write(), Position::new(0, 2)),
            Ok(None)
        );
        assert_eq!(
            gobang.next_round(Piece::black(), Position::new(2, 2)),
            Ok(None)
        );
        assert_eq!(
            gobang.next_round(Piece::write(), Position::new(0, 3)),
            Ok(None)
        );
        assert_eq!(
            gobang.next_round(Piece::black(), Position::new(3, 3)),
            Ok(None)
        );
        assert_eq!(
            gobang.next_round(Piece::write(), Position::new(0, 4)),
            Ok(None)
        );
        assert_eq!(
            gobang.next_round(Piece::black(), Position::new(4, 4)),
            Ok(Some(Camp::Black))
        );

        assert_eq!(
            gobang.next_round(Piece::write(), Position::new(0, 5)),
            Err(Error::GameIsOver)
        );
    }

    #[test]
    fn board() {
        let mut board = Board::default();
        let positions = board.board();
        let mut expected: [u16; board_cap()] = [
            2, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
            1,
        ];
        assert_eq!(
            board.next_round(Piece::black(), Position::new(0, 0)),
            Ok(None)
        );
        assert_eq!(board.board(), expected);
        assert_eq!(
            board.next_round(Piece::write(), Position::new(0, 1)),
            Ok(None)
        );
        expected[16] = 3;
        assert_eq!(board.board(), expected);
    }

    #[test]
    fn bad_ending() {
        let positions1: [u16; board_cap()] = [
            2, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
            1,
        ];
        let board1 = Board::bad_ending(&positions1);
        assert!(Board::bad_ending(&positions1).is_ok());
        assert_eq!(board1.unwrap().board(), positions1);

        let positions2: [u16; board_cap()] = [
            2, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 3, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
            1,
        ];
        let board2 = Board::bad_ending(&positions2);
        assert!(board2.is_ok());
        assert_eq!(board2.unwrap().board(), positions2);
    }
}
