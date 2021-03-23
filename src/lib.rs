//! # Minesweeper
//!
//! `minesweeper` is a library to handle the logic of the video game of the same name.

use rand::seq::IteratorRandom;
use std::fmt::{self, Display};
use std::ops::{Index, IndexMut};

/// Minesweeper game.
pub struct Minesweeper {
    config: Config,
    board: Board,
}

impl Minesweeper {
    /// Create a new Minesweeper game.
    pub fn new(config: Config) -> Minesweeper {
        Minesweeper {
            board: Board::new(config.height as usize, config.width as usize),
            config,
        }
    }

    /// Play a turn of the game.
    pub fn play(&mut self, turn: Turn) {
        // Initialize the board on first play
        if !self.board.initialized {
            self.board.init(self.config.bombs, turn.pos);
        }

        // Play the turn on the board
        self.board.play(turn);
    }
}

impl Display for Minesweeper {
    /// Display the game.
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        for _ in 0..(self.board.width() - 1) {
            write!(f, " ")?;
        }
        writeln!(f, "{:04}", self.config.bombs - self.board.flagged)?;
        write!(f, "{}", self.board)
    }
}

/// Minesweeper configuration.
pub struct Config {
    height: u8,
    width: u8,
    bombs: u16,
}

impl Config {
    /// Create a new Config.
    ///
    /// Defaults to the BEGINNER configuration.
    pub fn new(height: u8, width: u8, bombs: u16) -> Config {
        Config {
            height,
            width,
            bombs,
        }
    }

    /// Create a beginner Config.
    pub fn beginner() -> Config {
        Config::new(9, 9, 10)
    }

    /// Create an intermediate Config.
    pub fn intermediate() -> Config {
        Config::new(16, 16, 40)
    }

    /// Create an expert Config.
    pub fn expert() -> Config {
        Config::new(16, 32, 99)
    }
}

/// Board on which the game is played.
struct Board {
    tiles: Vec<Vec<Tile>>,
    initialized: bool,
    flagged: u16,
}

// Accessors/mutators for Board
impl Board {
    /// Borrow the tile at a position.
    fn get(&self, pos: Position) -> Option<&Tile> {
        self.tiles.get(pos.0)?.get(pos.1)
    }

    /// Mutably borrow the tile at a position.
    fn get_mut(&mut self, pos: Position) -> Option<&mut Tile> {
        self.tiles.get_mut(pos.0)?.get_mut(pos.1)
    }

    /// Get the board height.
    fn height(&self) -> usize {
        self.tiles.len()
    }

    /// Get the board width.
    fn width(&self) -> usize {
        self.tiles[0].len()
    }
}

impl Index<Position> for Board {
    type Output = Tile;

    fn index(&self, pos: Position) -> &Self::Output {
        &self.tiles[pos.0][pos.1]
    }
}

impl IndexMut<Position> for Board {
    fn index_mut(&mut self, pos: Position) -> &mut Self::Output {
        &mut self.tiles[pos.0][pos.1]
    }
}

// Game logic for Board
impl Board {
    /// Create a new Board.
    ///
    /// Initially, all tiles on the board aren't set.
    fn new(height: usize, width: usize) -> Board {
        Board {
            tiles: vec![vec![Tile::Hidden(State::Empty); width]; height],
            initialized: false,
            flagged: 0,
        }
    }

    /// Initialize the board.
    fn init(&mut self, bombs: u16, pos: Position) {
        let (height, width) = (self.height(), self.width());

        // Randomly choose locations of bombs:
        // - create a range for flattened board
        // - remove one tile representing `pos`
        // - randomly choose positions for the bombs
        // - increment all positions after `pos`
        // NOTE: we cannot choose `pos` since the first reveal mustn't fail
        let bombs = (0..(height * width - 1))
            .choose_multiple(&mut rand::thread_rng(), bombs as usize)
            .into_iter()
            .map(|x| x + (x >= (pos.0 * width + pos.1)) as usize);

        // Set bombs on board
        for bomb in bombs {
            let i = bomb % height;
            let j = (bomb - i) / width;
            self[Position(i, j)] = Tile::Hidden(State::Bomb);
        }

        // Mark board as initialized
        self.initialized = true;
    }

    /// Play a turn.
    fn play(&mut self, turn: Turn) {
        // Perform action on tile
        match turn.action {
            Action::Reveal => self.reveal(turn.pos),
            Action::Explore => self.explore(turn.pos),
            Action::Flag => self.flag(turn.pos),
            Action::Mark => self.mark(turn.pos),
        }
    }

    /// Reveal a tile.
    ///
    /// In the event a zero is revealed, explore the tile.
    ///
    /// # Notes
    ///
    /// While this does indirectly recurse, it will eventually terminate.
    fn reveal(&mut self, pos: Position) {
        // Reveal tile at `pos`
        if let Some(Tile::Hidden(state)) = self.get_mut(pos) {
            match state {
                State::Empty => self[pos] = Tile::Revealed(self.adjacent(pos)),
                State::Bomb => (), // TODO: handle revealing a bomb
            }
        } else {
            // Return early if:
            // - `pos` is out of bounds
            // - tile at `pos` is not hidden
            // NOTE: this is needed to terminate recursion
            return;
        }

        // NOTE: after this point, `pos` is guaranteed to be in bounds

        // Explore tile if revealed a zero
        if let Tile::Revealed(0) = self[pos] {
            self.explore(pos);
        }
    }

    /// Explore a tile.
    ///
    /// This will cause all adjacent tiles to be revealed.
    fn explore(&mut self, pos: Position) {
        // FIXME: when should explore be allowed?
        for i in vec![-1, 0, 1] {
            for j in vec![-1, 0, 1] {
                // Extract row and col
                let row = (pos.0 as isize + i) as usize;
                let col = (pos.1 as isize + j) as usize;

                // Perform bounds check
                if row >= self.height() || col >= self.width() {
                    continue;
                }

                // Reveal adjacent tile
                self.reveal(Position(row, col));
            }
        }
    }

    /// Flag a tile.
    fn flag(&mut self, pos: Position) {
        if let Some(tile) = self.get_mut(pos) {
            match tile.clone() {
                Tile::Hidden(state) | Tile::Marked(state) => {
                    self[pos] = Tile::Flagged(state);
                    self.flagged += 1;
                }
                Tile::Flagged(state) => {
                    self[pos] = Tile::Hidden(state);
                    self.flagged -= 1;
                }
                _ => (),
            }
        }
    }

    /// Mark a tile.
    fn mark(&mut self, pos: Position) {
        if let Some(tile) = self.get_mut(pos) {
            match tile.clone() {
                Tile::Hidden(state) => {
                    self[pos] = Tile::Marked(state);
                }
                Tile::Flagged(state) => {
                    self.flag(pos); // remove flag before marking
                    self[pos] = Tile::Marked(state);
                }
                Tile::Marked(state) => {
                    self[pos] = Tile::Hidden(state);
                }
                _ => (),
            }
        }
    }

    /// Count adjacent bombs.
    fn adjacent(&self, pos: Position) -> u8 {
        let mut count = 0;

        for i in vec![-1, 0, 1] {
            for j in vec![-1, 0, 1] {
                // Skip counting self
                if i == 0 && j == 0 {
                    continue;
                }

                // Extract row and col
                let row = (pos.0 as isize + i) as usize;
                let col = (pos.1 as isize + j) as usize;

                // Perform bounds check
                if row >= self.height() || col >= self.width() {
                    continue;
                }

                // Count bombs
                if let Some(tile) = self.get(Position(row, col)) {
                    match tile {
                        Tile::Hidden(State::Bomb)
                        | Tile::Flagged(State::Bomb)
                        | Tile::Marked(State::Bomb) => count += 1,
                        _ => (),
                    }
                }
            }
        }

        count
    }
}

impl Display for Board {
    /// Display the game board.
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        // Print top border
        write!(f, "┌")?;
        for _ in 0..self.width() {
            write!(f, "──")?;
        }
        writeln!(f, "─┐")?;

        // Print each row of the board
        for row in self.tiles.iter() {
            write!(f, "│")?;
            for tile in row.iter() {
                write!(f, " {}", tile)?;
            }
            writeln!(f, " │")?;
        }

        // Print bottom border
        write!(f, "└")?;
        for _ in 0..self.width() {
            write!(f, "──")?;
        }
        write!(f, "─┘")
    }
}

/// A tile of the board.
#[derive(Clone, Debug, PartialEq)]
enum Tile {
    Hidden(State),
    Flagged(State),
    Marked(State),
    Revealed(u8),
}

impl Display for Tile {
    /// Display a tile.
    ///
    /// | Tile          | Char |
    /// | ------------- | ---- |
    /// | `Hidden(_)`   | `◻`  |
    /// | `Flagged(_)`  | `⚑`  |
    /// | `Marked(_)`   | `⚐`  |
    /// | `Revealed(0)` | ` `  |
    /// | `Revealed(n)` | `n`  |
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Tile::Hidden(_) => write!(f, "◻"),
            Tile::Flagged(_) => write!(f, "⚑"),
            Tile::Marked(_) => write!(f, "⚐"),
            Tile::Revealed(0) => write!(f, " "),
            Tile::Revealed(n) => write!(f, "{}", n),
        }
    }
}

/// The state of a tile.
#[derive(Clone, Copy, Debug, PartialEq)]
enum State {
    Empty,
    Bomb,
}

/// An action the board position to perform it on.
#[derive(Clone)]
pub struct Turn {
    action: Action,
    pos: Position,
}

impl Turn {
    /// Create a new Turn.
    pub fn new(action: Action, pos: Position) -> Turn {
        Turn { action, pos }
    }
}

/// Actions performed on a board tile.
#[derive(Clone)]
pub enum Action {
    Reveal,
    Explore,
    Flag,
    Mark,
}

/// A position on the board.
#[derive(Clone, Copy)]
pub struct Position(pub usize, pub usize);

#[cfg(test)]
mod tests {
    use super::*;

    fn setup() -> Minesweeper {
        let config = Config::new(5, 5, 1);
        Minesweeper::new(config)
    }

    #[test]
    fn is_initially_nil() {
        let game = setup();

        for row in game.board.tiles {
            for tile in row {
                assert_eq!(tile, Tile::Hidden(State::Empty));
            }
        }
    }

    #[test]
    fn has_correct_number_of_bombs() {
        let mut game = setup();
        let turn = Turn::new(Action::Reveal, Position(2, 2));
        game.play(turn.clone());

        let mut bombs = 0;
        for row in game.board.tiles {
            for tile in row {
                if let Tile::Hidden(State::Bomb) = tile {
                    bombs += 1;
                }
            }
        }
        assert_eq!(bombs, game.config.bombs);
    }

    #[test]
    fn do_reveal_works() {
        let mut game = setup();

        let turn = Turn::new(Action::Reveal, Position(2, 2));
        game.play(turn.clone());
        assert!(matches!(game.board.get(turn.pos), Some(Tile::Revealed(_))));
    }

    #[test]
    fn do_flag_works() {
        let mut game = setup();

        let turn = Turn::new(Action::Flag, Position(2, 2));
        game.play(turn.clone());
        assert!(matches!(game.board.get(turn.pos), Some(Tile::Flagged(_))));
    }

    #[test]
    fn do_mark_works() {
        let mut game = setup();

        let turn = Turn::new(Action::Mark, Position(2, 2));
        game.play(turn.clone());
        assert!(matches!(game.board.get(turn.pos), Some(Tile::Marked(_))));
    }
}
