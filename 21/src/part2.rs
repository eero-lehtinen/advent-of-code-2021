use std::collections::HashMap;

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
struct Game {
	players: [Player; 2],
}

impl Game {
	const fn new(p1: Player, p2: Player) -> Self {
		Self { players: [p1, p2] }
	}
}

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
struct Player {
	pos: i32,
	score: i32,
}

impl Player {
	const fn new(pos: i32) -> Self {
		Self {
			pos: pos - 1,
			score: 0,
		}
	}

	fn mv(&mut self, spaces: i32) {
		self.pos += spaces;
		self.pos %= 10;
		self.score += self.pos + 1;
	}
}

const WINNING_SCORE: i32 = 21;

fn main() {
	let mut game_map = HashMap::new();
	game_map.insert(Game::new(Player::new(4), Player::new(9)), 1u64);

	let mut wins = [0, 0];

	let mut turn = 0;

	let possible_dice = (1..=3)
		.flat_map(|a| (1..=3).flat_map(move |b| (1..=3).map(move |c| a + b + c)))
		.collect::<Vec<_>>();

	println!("{:?}", possible_dice);

	while !game_map.is_empty() {
		let old_game_map = game_map;
		game_map = HashMap::new();

		let p_idx: usize = turn % 2;

		for (game, game_count) in &old_game_map {
			for die in &possible_dice {
				let mut new_game = game.clone();
				new_game.players[p_idx].mv(*die);
				if new_game.players[p_idx].score >= WINNING_SCORE {
					wins[p_idx] += game_count;
				} else {
					*game_map.entry(new_game.clone()).or_insert(0) += game_count;
				}
			}
		}

		turn += 1;
	}

	println!("wins: {:?}", wins);
}
