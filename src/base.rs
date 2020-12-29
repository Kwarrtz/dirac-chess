#![allow(dead_code)]

use std::collections::HashMap;
use std::fmt::{Debug};
use std::str::FromStr;

use num_traits::{FromPrimitive};
use num_derive::{FromPrimitive,ToPrimitive};

const FILES: [File;8] = [FA,FB,FC,FD,FE,FF,FG,FH];
const RANKS: [Rank;8] = [R1,R2,R3,R4,R5,R6,R7,R8];
const BACK_RANK: [(File,Piece);8] = [(FA,Rook), (FB,Bishop), (FC,Knight), (FD,Queen), (FE,King), (FF,Knight), (FG,Bishop), (FH,Rook)];

#[derive(Debug, PartialEq, Eq, FromPrimitive, ToPrimitive, Clone, Copy, Hash)]
pub enum Rank { R1 = 1, R2, R3, R4, R5, R6, R7, R8 }
use Rank::*;

#[derive(Debug, PartialEq, Eq, FromPrimitive, ToPrimitive, Clone, Copy, Hash)]
pub enum File { FA = 1, FB, FC, FD, FE, FF, FG, FH }
use File::*;

#[derive(Debug, PartialEq, Eq, Clone, Copy, Hash)]
pub struct Square { file: File, rank: Rank }

impl Square {
    pub fn new(file: File, rank: Rank) -> Self {
        Square { file, rank }
    }

    pub fn over(self, (f,r): (i8,i8)) -> Option<Square> {
        Some(Square::new(
            FromPrimitive::from_i8(self.file as i8 + f)?, 
            FromPrimitive::from_i8(self.rank as i8 + r)?)
        )
    }
}

impl ToString for Rank {
    fn to_string(&self) -> String {
        (*self as u8).to_string()
    }
}

impl ToString for File {
    fn to_string(&self) -> String {
        String::from(["a","b","c","d","e","f","g","h"][*self as usize - 1])
    }
}

impl ToString for Square {
    fn to_string(&self) -> String {
        self.file.to_string() + &self.rank.to_string()
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct MoveParseError;

impl From<std::option::NoneError> for MoveParseError {
    fn from(_: std::option::NoneError) -> MoveParseError {
        MoveParseError
    }
}

impl FromStr for Square {
    type Err = MoveParseError;

    fn from_str(s: &str) -> Result<Square, MoveParseError> {
        if s.len() != 2 { return Err(MoveParseError); }
        let mut chars = s.chars();

        let file = match chars.next()?.to_ascii_lowercase() {
            'a' => FA,
            'b' => FB,
            'c' => FC,
            'd' => FD,
            'e' => FE,
            'f' => FF,
            'g' => FG,
            'h' => FH,
            _ => { return Err(MoveParseError) }
        };

        let rank = match chars.next()? {
            '1' => R1,
            '2' => R2,
            '3' => R3,
            '4' => R4,
            '5' => R5,
            '6' => R6,
            '7' => R7,
            '8' => R8,
            _ => { return Err(MoveParseError) }
        };

        Ok(Square::new(file,rank))
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Move(Square, Square);

impl FromStr for Move {
    type Err = MoveParseError;

    fn from_str(s: &str) -> Result<Move, MoveParseError> {
        let mut squares = s.split(' ').map(|ss| ss.parse());
        let start = squares.next()??;
        let end = squares.next()??;
        if squares.next() != None { return Err(MoveParseError); }
        Ok(Move(start,end))
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum MoveType {
    Regular,
    PawnRegular,
    PawnJump,
    EnPassant,
    Promotion,
    LongCastle,
    ShortCastle
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum MoveOutcome {
    Legal,
    Illegal,
    WonBy(Player)
}

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub enum Piece { King, Queen, Rook, Bishop, Knight, Pawn }
use Piece::*;

impl Piece {
    fn draw(&self, player: Player) -> char {
        let s = match self {
            King => 'k',
            Queen => 'q',
            Rook => 'r',
            Bishop => 'b',
            Knight => 'n',
            Pawn => 'p'
        };
    
        match player {
            Black => s,
            White => s.to_ascii_uppercase()
        }
    }
}

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub enum Player { Black = 0, White }
use Player::*;

impl Player {
    pub fn other(&self) -> Player {
        match self {
            Black => White,
            White => Black
        }
    }

    fn back_rank(&self) -> Rank {
        match self {
            White => R1,
            Black => R8
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Position {
    pieces: HashMap<Square, (Player, Piece)>,
    pub en_passant: Option<File>,
    can_castle_long: [bool;2],
    can_castle_short: [bool;2]
}

impl Position {
    pub fn starting() -> Self {
        let mut pieces = HashMap::new();

        for (rank,player) in &[(R1,White),(R8,Black)] {
            for (file,piece) in &BACK_RANK {
                pieces.insert(Square::new(*file,*rank), (*player,*piece));
            }
        }
        
        for (rank,player) in &[(R2,White), (R7,Black)] {
            for file in &FILES {
                pieces.insert(Square::new(*file,*rank), (*player, Pawn));
            }
        }

        Position { 
            pieces,
            en_passant: None,
            can_castle_long: [true, true],
            can_castle_short: [true, true]
        }
    }

    pub fn at(&self, sq: Square) -> Option<(Player,Piece)> {
        self.pieces.get(&sq).cloned()
    }

    pub fn player_at(&self, sq: Square) -> Option<Player> {
        self.at(sq).map(|(player,_)| player)
    }

    fn available(&self, sq: Square, player: Player) -> bool {
        match self.at(sq) {
            Some((p,_)) => p == player,
            None => true
        }
    }

    fn cast_ray(&self, square: Square, direction: (i8,i8), player: Player) -> MoveRay {
        MoveRay {
            position: self,
            current: square,
            direction,
            player,
            done: false
        }
    }

    pub fn attacked_by(&self, player: Player) -> Vec<Square> {
        let mut squares = vec![];
        for (&square, &(p, piece)) in self.pieces.iter() {
            if p == player && piece != King {
                let moves = self.legal_moves(player, piece, square);
                let attacking_moves = moves.iter().filter(|(_,&t)| t != MoveType::PawnRegular);
                squares.extend(attacking_moves.map(|(&s,_)| s));
            }
        }

        squares
    }

    pub fn legal_moves(&self, player: Player, piece: Piece, square: Square) -> HashMap<Square,MoveType> {
        match piece {
            Rook => {
                [(1,0),(0,1),(-1,0),(0,-1)].iter().map(|&dir| {
                    self.cast_ray(square, dir, player)
                }).flatten().map(|s| (s, MoveType::Regular)).collect()
            },

            Bishop => {
                [(1,1),(1,-1),(-1,1),(-1,-1)].iter().map(|&dir| {
                    self.cast_ray(square, dir, player)
                }).flatten().map(|s| (s, MoveType::Regular)).collect()
            },

            Queen => {
                [(1,0),(0,1),(-1,0),(0,-1),(1,1),(1,-1),(-1,1),(-1,-1)].iter().map(|&dir| {
                    self.cast_ray(square, dir, player)
                }).flatten().map(|s| (s, MoveType::Regular)).collect()
            },

            King => {
                let mut moves: HashMap<Square, MoveType> = [(1,0),(0,1),(-1,0),(0,-1),(1,1),(1,-1),(-1,1),(-1,-1)].iter().map(|&dir| {
                    square.over(dir).filter(|&sq| self.player_at(sq) != Some(player))
                }).flatten().map(|s| (s, MoveType::Regular)).collect();

                let attacked = self.attacked_by(player.other());

                if  [FC,FD,FE].iter().map(|&f| Square::new(f, player.back_rank()))
                        .all(|s| (s.file == FE || self.at(s) == None) && attacked.iter().all(|&a| s != a))
                && self.can_castle_long[player as usize] {
                    moves.insert(Square::new(FC, player.back_rank()), MoveType::LongCastle);
                }

                if  [FE,FF,FG].iter().map(|&f| Square::new(f, player.back_rank()))
                        .all(|s| (s.file == FE || self.at(s) == None) && attacked.iter().all(|&a| s != a))
                && self.can_castle_short[player as usize] {
                    moves.insert(Square::new(FG, player.back_rank()), MoveType::ShortCastle);
                }

                moves
            },

            Knight => {
                [(1,2),(2,1),(-1,2),(-2,1),(1,-2),(2,-1),(-1,-2),(-2,-1)].iter().map(|&dir| {
                    square.over(dir).filter(|&sq| self.player_at(sq) != Some(player))
                }).flatten().map(|s| (s, MoveType::Regular)).collect()
            }

            Pawn => {
                let mut moves: HashMap<Square, MoveType>;

                match player {
                    White => {
                        let dirs: &[((i8,i8), MoveType)] = 
                            if square.rank == R2 { &[((0,1), MoveType::PawnRegular), ((0,2), MoveType::PawnJump)] } 
                            else { &[((0,1), MoveType::PawnRegular)] };
                        moves = dirs.iter().map(|&(dir,typ)| {
                            square.over(dir).filter(|&sq| self.player_at(sq) == None).map(|s| (s,typ))
                        }).flatten().collect();

                        let captures = [(1,1), (-1,1)].iter().map(|&dir| {
                            square.over(dir).and_then(|sq| 
                                if self.player_at(sq) == Some(Black) {
                                    Some((sq, MoveType::Regular))
                                } else if Some(sq) == self.en_passant.map(|f| Square::new(f, R6)) {
                                    Some((sq, MoveType::EnPassant))
                                } else {
                                    None
                                }
                            )
                        }).flatten();

                        moves.extend(captures);
                    },

                    Black => {
                        let dirs: &[((i8,i8), MoveType)] = 
                            if square.rank == R7 { &[((0,-1), MoveType::PawnRegular), ((0,-2), MoveType::PawnJump)] } 
                            else { &[((0,-1), MoveType::PawnRegular)] };
                        moves = dirs.iter().map(|&(dir,typ)| {
                            square.over(dir).filter(|&sq| self.player_at(sq) == None).map(|s| (s,typ))
                        }).flatten().collect();

                        let captures = [(1,-1), (-1,-1)].iter().map(|&dir| {
                            square.over(dir).and_then(|sq| 
                                if self.player_at(sq) == Some(White) {
                                    Some((sq, MoveType::Regular))
                                } else if Some(sq) == self.en_passant.map(|f| Square::new(f, R3)) {
                                    Some((sq, MoveType::EnPassant))
                                } else {
                                    None
                                }
                            )
                        }).flatten();

                        moves.extend(captures);
                    }
                };

                moves
            }
        }
    }

    pub fn make_move(&mut self, player: Player, Move(from, to): Move) -> MoveOutcome {
        match self.at(from) {
            None => MoveOutcome::Illegal,
            Some((player_at, piece)) => {
                if player_at == player {
                    if let Some(move_type) = self.legal_moves(player, piece, from).get(&to) {
                        // clear en passant capture possibility
                        self.en_passant = None;

                        // move piece
                        let val = self.pieces.remove(&from).unwrap();
                        let captured = self.pieces.insert(to, val);

                        // check for moves that would eliminate player's castling rights
                        if from == Square::new(FA, player.back_rank()) 
                        || from == Square::new(FE, player.back_rank()) {
                            self.can_castle_long[player as usize] = false;
                        }

                        if from == Square::new(FH, player.back_rank()) 
                        || from == Square::new(FE, player.back_rank()) {
                            self.can_castle_short[player as usize] = false;
                        }

                        // check for captures that would eliminate other player's castling rights
                        if to == Square::new(FA, player.other().back_rank()) {
                            self.can_castle_long[player.other() as usize] = false;
                        }

                        if to == Square::new(FH, player.other().back_rank()) {
                            self.can_castle_short[player.other() as usize] = false;
                        }

                        // check if the king has been captured
                        if let Some((_, King)) = captured { return MoveOutcome::WonBy(player); }

                        match move_type {
                            MoveType::ShortCastle => {
                                let rank = match player { White => R1, Black => R8 };
                                self.pieces.remove(&Square::new(FH, rank));
                                self.pieces.insert(Square::new(FF, rank), (player, Rook));
                            }

                            MoveType::LongCastle => {
                                let rank = match player { White => R1, Black => R8 };
                                self.pieces.remove(&Square::new(FA, rank));
                                self.pieces.insert(Square::new(FD, rank), (player, Rook));
                            }

                            MoveType::PawnJump => {
                                self.en_passant = Some(to.file);
                            }

                            MoveType::EnPassant => {
                                let rank = match player { White => R5, Black => R4 };
                                self.pieces.remove(&Square::new(to.file, rank));
                            }

                            MoveType::Promotion => {
                                self.pieces.insert(to, (player, Queen));
                            }

                            _ => { }
                        };

                        MoveOutcome::Legal
                    } else {
                        MoveOutcome::Illegal
                    }
                } else {
                    MoveOutcome::Illegal
                }
            }
        }
    }

    pub fn draw(&self) -> String {
        let mut board = String::from("");

        for &rank in RANKS.iter().rev() {
            for &file in &FILES {
                board.push(match self.at(Square::new(file,rank)) {
                    Some((player, piece)) => piece.draw(player),
                    None => '*'
                });
                board.push(' ');
            }
            board.push('\n');
        }

        board
    }
}

#[derive(Clone, Copy)]
struct MoveRay<'a> { 
    position: &'a Position, 
    player: Player, 
    current: Square, 
    direction: (i8,i8), 
    done: bool 
}

impl<'a> Iterator for MoveRay<'a> {
    type Item = Square;

    fn next(&mut self) -> Option<Square> {
        if self.done {
            None
        } else {
            match self.current.over(self.direction) {
                None => None,
                Some(sq) => {
                    self.current = sq;
                    match self.position.at(sq) {
                        Some((player,_)) => {
                            self.done = true;
                            if player == self.player {
                                None
                            } else {
                                Some(sq)
                            }
                        },
                        None => Some(sq)
                    }
                }
            }
        }
    }
}

struct GameState {
    positions: Vec<(Position, f32)>
}