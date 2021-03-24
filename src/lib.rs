//! # Minesweeper
//!
//! `minesweeper` is a library to handle the logic of the video game of the same name.

use rand::prelude::*;
use std::cmp;
use std::fmt::{self, Display};
use std::ops::{Index, IndexMut};

/// Minesweeper game.
pub struct Minesweeper {
    board: Board,
}

impl Minesweeper {
    /// Create a new Minesweeper game.
    pub fn new(config: Config) -> Minesweeper {
        Minesweeper {
            board: Board::new(config),
        }
    }

    /// Play a turn of the game.
    pub fn play(&mut self, turn: Turn) {
        self.board.play(turn);
    }

    /// Check if the game is over.
    pub fn over(&self) -> bool {
        self.board.over()
    }
}

impl Display for Minesweeper {
    /// Display the game.
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let bombs_remaining = self.board.bombs as i16 - self.board.flagged as i16;
        write!(f, "{}", " ".repeat(self.board.width()))?;
        writeln!(f, "{:04}", bombs_remaining)?;
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
#[derive(Debug)]
struct Board {
    tiles: Vec<Vec<Tile>>,
    unrevealed: u16,
    bombs: u16,
    flagged: u16,
}

// Accessors/mutators for Board
impl Board {
    /// Check if the game is initialized.
    fn initialized(&self) -> bool {
        (self.unrevealed as usize) < self.height() * self.width()
    }

    /// Get the board height.
    fn height(&self) -> usize {
        self.tiles.len()
    }

    /// Get the board width.
    fn width(&self) -> usize {
        self.tiles[0].len()
    }
    /// Borrow the tile at a position.
    fn get(&self, pos: Position) -> Option<&Tile> {
        self.tiles.get(pos.0)?.get(pos.1)
    }

    /// Mutably borrow the tile at a position.
    fn get_mut(&mut self, pos: Position) -> Option<&mut Tile> {
        self.tiles.get_mut(pos.0)?.get_mut(pos.1)
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
    fn new(config: Config) -> Board {
        let Config {
            height,
            width,
            bombs,
        } = config;
        let height = height as usize;
        let width = width as usize;
        let area = (height * width) as u16;

        Board {
            tiles: vec![vec![Tile::Hidden(State::Empty); width]; height],
            bombs: cmp::min(bombs, area - 1),
            unrevealed: area,
            flagged: 0,
        }
    }

    /// Initialize the board.
    fn init(&mut self, pos: Position) {
        let width = self.width();

        // Closure to convert from row and col to index
        let flatten = |row, col| row * width + col;

        // Closure to convert from index to row and col
        let unflatten = |x: &usize| {
            let row = x / width;
            let col = x % width;
            (row, col)
        };

        // Closure to determine if a given row and col is adjacent to`pos`
        // NOTE: a maximum of `filterable` adjacent bombs can be filtered
        let mut filterable = cmp::min(
            self.unrevealed - self.bombs - 1,
            self.adjacent(pos, State::Empty) as u16,
        );
        let mut is_adjacent = |(row, col)| {
            let delta = (row as isize - pos.0 as isize, col as isize - pos.1 as isize);
            let filtered = filterable > 0 && delta.0.abs() <= 1 && delta.1.abs() <= 1;
            if filtered {
                filterable -= 1;
            }
            filtered
        };

        // Randomly choose locations of bombs:
        // - create a vector of flattened board
        // - shuffle the board for bomb selection
        // - filter out `pos` since first guess can't be a bomb
        // - filter out some elements near `pos`
        // - take bombs from beginning of remaining tiles
        let mut bombs: Vec<_> = (0..self.unrevealed as usize).collect();
        bombs.shuffle(&mut rand::thread_rng());
        let bombs = bombs
            .into_iter()
            .filter(|x| *x != flatten(pos.0, pos.1))
            .filter(|x| !is_adjacent(unflatten(x)))
            .take(self.bombs as usize);

        // Set bombs on board
        for bomb in bombs {
            let (row, col) = unflatten(&bomb);
            self[Position(row, col)] = Tile::Hidden(State::Bomb);
        }
    }

    /// Check if the game is over.
    fn over(&self) -> bool {
        self.unrevealed == self.bombs
    }

    /// Play a turn.
    fn play(&mut self, turn: Turn) {
        // Validate turn position
        if let None = self.get(turn.pos) {
            return;
        }

        // Initialize the board on first play
        if !self.initialized() {
            self.init(turn.pos);
        }

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
    /// While this does indirectly recurse, it will eventually terminate.
    fn reveal(&mut self, pos: Position) {
        // Reveal tile at `pos`
        if let Some(Tile::Hidden(state)) = self.get_mut(pos) {
            match state {
                State::Empty => self[pos] = Tile::Revealed(self.adjacent(pos, State::Bomb)),
                State::Bomb => {
                    eprintln!("warning: bomb revealed");
                    return;
                } // TODO: handle revealing a bomb
            }
            self.unrevealed -= 1;
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
    ///
    /// # Panics
    ///
    /// Will panic if `pos` is out of bounds.
    fn explore(&mut self, pos: Position) {
        // Only explore tiles that are revealed
        // FIXME: only allow explore if adjacent flags met
        if let Tile::Revealed(_) = self[pos] {
            for i in vec![-1, 0, 1] {
                for j in vec![-1, 0, 1] {
                    // Extract row and col
                    let row = (pos.0 as isize + i) as usize;
                    let col = (pos.1 as isize + j) as usize;

                    // Reveal adjacent tile (performs bounds check)
                    self.reveal(Position(row, col));
                }
            }
        }
    }

    /// Flag a tile.
    ///
    /// # Panics
    ///
    /// Will panic if `pos` is out of bounds.
    fn flag(&mut self, pos: Position) {
        match self[pos] {
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

    /// Mark a tile.
    ///
    /// # Panics
    ///
    /// Will panic if `pos` is out of bounds.
    fn mark(&mut self, pos: Position) {
        match self[pos] {
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

    /// Count state of adjacent tiles.
    fn adjacent(&self, pos: Position, state: State) -> u8 {
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

                // Count bombs (performs bounds check)
                if let Some(tile) = self.get(Position(row, col)) {
                    count += (tile.state() == state) as u8;
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
        writeln!(f, "┌{}─┐", "──".repeat(self.width()))?;

        // Print each row of the board
        for row in self.tiles.iter() {
            write!(f, "│")?;
            for tile in row.iter() {
                write!(f, " {}", tile)?;
            }
            writeln!(f, " │")?;
        }

        // Print bottom border
        write!(f, "└{}─┘", "──".repeat(self.width()))
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

impl Tile {
    /// Get the state of a tile.
    fn state(&self) -> State {
        match self {
            Tile::Hidden(state) | Tile::Flagged(state) | Tile::Marked(state) => *state,
            _ => State::Empty,
        }
    }
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
#[derive(Clone, Debug)]
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
#[derive(Clone, Debug)]
pub enum Action {
    Reveal,
    Explore,
    Flag,
    Mark,
}

/// A position on the board.
#[derive(Clone, Copy, Debug, PartialEq)]
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
        assert_eq!(bombs, game.board.bombs);
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
