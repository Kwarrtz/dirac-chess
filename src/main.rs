#![feature(try_trait)]

mod base;
use base::*;
use std::io::{self,Write};

fn main() {
    let mut move_num: u32 = 1;
    let mut current_player = Player::White;
    let mut board = Position::starting();
    println!("{}", board.draw());
    loop {
        print!("({}) {:?} to move: ", move_num, current_player);
        io::stdout().flush().unwrap();
        let mut move_in: String = String::new();
        io::stdin().read_line(&mut move_in).unwrap();
        let parsed_move = move_in.trim().parse::<Move>();
        match parsed_move {
            Err(_) => {
                println!("Invalid move syntax.");
            }

            Ok(mov) => {
                match board.make_move(current_player, mov) {
                    MoveOutcome::Legal => {
                        if current_player == Player::Black {
                            move_num += 1;
                        }
                        current_player = current_player.other();
                        println!("\n{}", board.draw());
                    }

                    MoveOutcome::Illegal => {
                        println!("Illegal move.");
                    }

                    MoveOutcome::WonBy(winner) => {
                        println!("\n{}\nGame over. {:?} wins.", board.draw(), winner);
                        break;
                    }
                }
            }
        }
    }
}
