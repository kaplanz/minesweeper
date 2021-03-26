//! # Minesweeper
//!
//! `minesweeper` is a library to handle the logic of the video game of the same name.

use rand::prelude::*;
use std::cmp;
use std::fmt::{self, Display};
use std::ops::{Index, IndexMut};

/// Minesweeper game.
#[derive(Debug)]
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

    /// Get the winner of the game.
    ///
    /// Returns `None` if the game is still ongoing.
    pub fn winner(&self) -> Option<bool> {
        self.board.winner()
    }
}

impl Display for Minesweeper {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let bombs_remaining = self.board.bombs as i16 - self.board.flagged as i16;
        write!(f, "{}", " ".repeat(self.board.width()))?;
        writeln!(f, "{:04}", bombs_remaining)?;
        write!(f, "{}", self.board)
    }
}

/// Minesweeper configuration.
#[derive(Debug)]
pub struct Config {
    height: u8,
    width: u8,
    bombs: u16,
}

impl Config {
    /// Create a new game Config.
    ///
    /// `bombs` is taken as a hint, and is constrained by the board dimensions.
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
///
/// Responsible for managing the placement of tiles and handling game logic.
#[derive(Debug)]
struct Board {
    tiles: Vec<Vec<Tile>>,
    detonation: Option<Position>,
    unrevealed: u16,
    bombs: u16,
    flagged: u16,
}

impl Board {
    /// Create a new Board.
    ///
    /// The newly created board is not yet initialized.
    /// Bombs are placed as the first turn is played.
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
            detonation: None,
            bombs: cmp::min(bombs, area - 1),
            unrevealed: area,
            flagged: 0,
        }
    }
}

impl Board {
    /// Check if the board is initialized.
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
    ///
    /// Performs bounds check, and returns `None` variant on invalid position.
    fn get(&self, pos: Position) -> Option<&Tile> {
        self.tiles.get(pos.0)?.get(pos.1)
    }

    /// Mutably borrow the tile at a position.
    ///
    /// Performs bounds check, and returns `None` variant on invalid position.
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

impl Board {
    /// Initialize the board.
    ///
    /// This function is responsible for determining the positions of bombs.
    ///
    /// Bombs are placed randomly with the following restrictions:
    /// - the first play,`pos`, is guaranteed to be safe.
    /// - if possible, `pos` will reveal a zero.
    ///   (i.e. bombs will avoid being placed adjacent to `pos`.)
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
            self.adjacent_state(pos, State::Empty) as u16,
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

    /// Play a turn of the game.
    ///
    /// Ensures the turn is at a valid position.
    ///
    /// Calls `init` to initialize the board on the first play.
    fn play(&mut self, turn: Turn) -> Option<&Tile> {
        // Perform bounds check
        if let None = self.get(turn.pos) {
            return None;
        }

        // Initialize the board on first play
        if !self.initialized() {
            self.init(turn.pos);
        }

        // Perform action on tile
        match turn.action {
            Action::Reveal => self.reveal(turn.pos),
            Action::Explore => self.explore(turn.pos, false),
            Action::Flag => self.flag(turn.pos),
            Action::Mark => self.mark(turn.pos),
        }
    }

    /// Check if the game is over.
    fn over(&self) -> bool {
        self.lost() || self.won()
    }

    /// Check if the game was lost.
    fn lost(&self) -> bool {
        self.detonation.is_some()
    }

    /// Check if the game was won.
    fn won(&self) -> bool {
        self.unrevealed == self.bombs
    }

    /// Get the winner of the game.
    ///
    /// Returns `None` if the game is still ongoing.
    fn winner(&self) -> Option<bool> {
        match self.over() {
            true => Some(self.won() && !self.lost()),
            false => None,
        }
    }

    /// Reveal a tile.
    ///
    /// Upon revealing a zero, `reveal` will explore the tile.
    ///
    /// This will lead to recursion if more zeros are revealed.
    fn reveal(&mut self, pos: Position) -> Option<&Tile> {
        // Reveal tile at `pos`
        if let Some(Tile::Hidden(state)) = self.get_mut(pos) {
            match state {
                State::Empty => self[pos] = Tile::Revealed(self.adjacent_state(pos, State::Bomb)),
                State::Bomb => {
                    self.detonate(pos);
                    return Some(&self[pos]);
                }
            }
            self.unrevealed -= 1;
        } else {
            // Return early if:
            // - `pos` is out of bounds
            // - tile at `pos` is not hidden
            // NOTE: this is the base case to terminate recursion
            return None;
        }

        // NOTE: after this point, `pos` is guaranteed to be in bounds

        // Explore tile if revealed a zero
        if let Tile::Revealed(0) = self[pos] {
            self.explore(pos, true);
        }

        Some(&self[pos])
    }

    /// Detonate a bomb.
    ///
    /// Causes the game to be lost, and thus terminate.
    /// Reveal the locations of all bombs.
    ///
    /// # Panics
    ///
    /// Will panic if `pos` is out of bounds.
    fn detonate(&mut self, pos: Position) {
        // Record location of detonation
        self.detonation = Some(pos);

        // Reveal all bombs
        for row in self.tiles.iter_mut() {
            for tile in row {
                if tile.state() == State::Bomb {
                    *tile = Tile::Detonated;
                }
            }
        }
    }

    /// Explore a tile.
    ///
    /// This will cause all adjacent tiles to be revealed.
    ///
    /// When called by the user, we must ensure the action is valid.
    /// For a user to explore, `pos` must be a revealed tile with the
    /// correct amount of adjacent flags.
    ///
    /// # Panics
    ///
    /// Will panic if `pos` is out of bounds.
    fn explore(&mut self, pos: Position, recursed: bool) -> Option<&Tile> {
        // Only explore tiles that are revealed
        if let Tile::Revealed(n) = self[pos] {
            // Only allow explore if correct amount of adjacent flags
            if recursed || self.adjacent_tile(pos, &Tile::Flagged(State::Bomb)) == n {
                for i in vec![-1, 0, 1] {
                    for j in vec![-1, 0, 1] {
                        // Extract row and col
                        let row = (pos.0 as isize + i) as usize;
                        let col = (pos.1 as isize + j) as usize;

                        // Reveal adjacent tile (performs bounds check)
                        self.reveal(Position(row, col));
                    }
                }

                return Some(&self[pos]);
            }
        }

        None
    }

    /// Flag a tile.
    ///
    /// # Panics
    ///
    /// Will panic if `pos` is out of bounds.
    fn flag(&mut self, pos: Position) -> Option<&Tile> {
        match self[pos] {
            Tile::Hidden(state) | Tile::Marked(state) => {
                self[pos] = Tile::Flagged(state);
                self.flagged += 1;
                Some(&self[pos])
            }
            Tile::Flagged(state) => {
                self[pos] = Tile::Hidden(state);
                self.flagged -= 1;
                Some(&self[pos])
            }
            _ => None,
        }
    }

    /// Mark a tile.
    ///
    /// # Panics
    ///
    /// Will panic if `pos` is out of bounds.
    fn mark(&mut self, pos: Position) -> Option<&Tile> {
        match self[pos] {
            Tile::Hidden(state) => {
                self[pos] = Tile::Marked(state);
                Some(&self[pos])
            }
            Tile::Flagged(state) => {
                self.flag(pos); // remove flag before marking
                self[pos] = Tile::Marked(state);
                Some(&self[pos])
            }
            Tile::Marked(state) => {
                self[pos] = Tile::Hidden(state);
                Some(&self[pos])
            }
            _ => None,
        }
    }

    /// Count adjacent tiles that match `state`.
    fn adjacent_state(&self, pos: Position, state: State) -> u8 {
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

    /// Count adjacent tiles whose discriminant matches `tile`.
    ///
    /// Only the variant discriminant itself is compared.
    /// The interior value (if applicable to the variant) is ignored.
    fn adjacent_tile(&self, pos: Position, tile: &Tile) -> u8 {
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
                if let Some(other) = self.get(Position(row, col)) {
                    count += (std::mem::discriminant(tile) == std::mem::discriminant(&other)) as u8;
                }
            }
        }

        count
    }
}

impl Display for Board {
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
    Detonated,
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
    /// | `Detonated`   | `☢︎`  |
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Tile::Hidden(_) => write!(f, "◻"),
            Tile::Flagged(_) => write!(f, "⚑"),
            Tile::Marked(_) => write!(f, "⚐"),
            Tile::Revealed(0) => write!(f, " "),
            Tile::Revealed(n) => write!(f, "{}", n),
            Tile::Detonated => write!(f, "☢︎"),
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
#[derive(Clone, Copy, Debug)]
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
    fn bounds_check_works() {
        let mut game = setup();

        let pos = Position(0, game.board.width());
        assert!(matches!(game.board.get(pos), None));
        let turn = Turn::new(Action::Reveal, pos);
        game.play(turn);

        let pos = Position(game.board.height(), 0);
        assert!(matches!(game.board.get(pos), None));
        let turn = Turn::new(Action::Reveal, pos);
        game.play(turn);
    }

    #[test]
    fn do_reveal_works() {
        let mut game = setup();

        let pos = Position(2, 2);
        let turn = Turn::new(Action::Reveal, pos);
        game.play(turn);

        assert!(matches!(game.board.get(pos), Some(Tile::Revealed(_))));
    }

    #[test]
    fn do_flag_works() {
        let mut game = setup();

        let pos = Position(2, 2);
        let turn = Turn::new(Action::Flag, pos);
        game.play(turn);

        assert!(matches!(game.board.get(pos), Some(Tile::Flagged(_))));
    }

    #[test]
    fn do_mark_works() {
        let mut game = setup();

        let pos = Position(2, 2);
        let turn = Turn::new(Action::Mark, pos);
        game.play(turn);

        assert!(matches!(game.board.get(pos), Some(Tile::Marked(_))));
    }

    #[test]
    fn win_game_works() {
        for _ in 0..128 {
            let mut game = setup();

            'outer: for row in 0..game.board.height() {
                for col in 0..game.board.width() {
                    let pos = Position(row, col);

                    if let Tile::Hidden(State::Empty) = game.board[pos] {
                        let turn = Turn::new(Action::Reveal, pos);
                        game.play(turn);
                    }

                    if game.over() {
                        break 'outer;
                    }
                }
            }

            assert!(game.winner().unwrap());
        }
    }

    #[test]
    fn lose_game_works() {
        for _ in 0..128 {
            let mut game = setup();

            for row in 0..game.board.height() {
                for col in 0..game.board.width() {
                    let pos = Position(row, col);
                    let tile = game.board[pos].clone();
                    let turn = Turn::new(Action::Reveal, pos);
                    game.play(turn);

                    if let Tile::Hidden(State::Bomb) = tile {
                        assert!(!game.winner().unwrap());
                    }
                }
            }
        }
    }

    #[test]
    fn bomb_avoidance_works() {
        let mut game = setup();
        game.board.bombs = 24;
        let turn = Turn::new(Action::Reveal, Position(0, 0));
        game.play(turn);
        assert!(game.over());

        let mut game = setup();
        game.board.bombs = 24;
        let turn = Turn::new(Action::Reveal, Position(0, 1));
        game.play(turn);
        assert!(game.over());

        let mut game = setup();
        game.board.bombs = 24;
        let turn = Turn::new(Action::Reveal, Position(1, 1));
        game.play(turn);
        assert!(game.over());

        let mut game = setup();
        game.board.bombs = 24;
        let turn = Turn::new(Action::Reveal, Position(1, 2));
        game.play(turn);
        assert!(game.over());

        let mut game = setup();
        game.board.bombs = 24;
        let turn = Turn::new(Action::Reveal, Position(2, 2));
        game.play(turn);
        assert!(game.over());
    }
}
