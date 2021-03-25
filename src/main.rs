use minesweeper::*;
use std::io::{self, Write};

fn main() {
    println!("Welcome to Minesweeper!");
    println!();

    let config = Config::beginner();
    let mut game = Minesweeper::new(config);

    while !game.over() {
        println!("{}", game);
        let turn = get_turn();
        game.play(turn);
    }

    println!("{}", game);
    match game.winner().unwrap() {
        true => println!("A winner is you!"),
        false => println!("Boom! You lose."),
    }
}

fn get_turn() -> Turn {
    loop {
        // Print prompt
        print!(">> ");
        io::stdout().flush().unwrap();

        // Get user input
        let mut input = String::new();
        io::stdin().read_line(&mut input).unwrap();

        // Process input
        let input: Vec<_> = input.split_whitespace().collect();
        if input.is_empty() {
            continue;
        }

        // Process command
        let action = match input[0] {
            "r" | "reveal" => Action::Reveal,
            "e" | "explore" => Action::Explore,
            "f" | "flag" => Action::Flag,
            "m" | "mark" => Action::Mark,
            "h" | "help" => {
                println!("USAGE:");
                println!("\tr, reveal  \tReveal a tile");
                println!("\te, explore \tExplore a tile");
                println!("\tf, flag    \tFlag a tile");
                println!("\tm, mark    \tMark a tile");
                println!("\th, help    \tPrints help information");
                continue;
            }
            _ => {
                eprintln!("error: command not found");
                continue;
            }
        };

        // Validate argument count
        if input.len() != 3 {
            eprintln!("error: invalid args");
            continue;
        }

        // Parse arguments
        let row = match input[1].parse::<usize>() {
            Ok(x) if x > 0 => x - 1,
            _ => {
                eprintln!("error: could not parse row");
                continue;
            }
        };
        let col = match input[2].parse::<usize>() {
            Ok(x) if x > 0 => x - 1,
            _ => {
                eprintln!("error: could not parse col");
                continue;
            }
        };
        let pos = Position(row, col);

        return Turn::new(action, pos);
    }
}
