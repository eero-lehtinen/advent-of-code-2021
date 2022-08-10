#[derive(Clone, Debug)]
struct Player {
	pos: i32,
	score: i32,
}

impl Player {
	fn new(pos: i32) -> Player {
		Player {
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

struct DeterministicDie {
	num: i32,
	rolls: i32,
}

impl DeterministicDie {
	fn new() -> DeterministicDie {
		DeterministicDie { num: 0, rolls: 0 }
	}

	fn roll(&mut self) -> i32 {
		self.rolls += 1;
		self.num += 1;
		if self.num > 100 {
			self.num = 1;
		}
		self.num
	}
}

fn main() {
	let mut players = vec![Player::new(4), Player::new(9)];

	let mut turn = 0;

	let mut die = DeterministicDie::new();

	loop {
		let turn_die_total: i32 = (0..3).map(|_| die.roll()).sum();

		players[turn].mv(turn_die_total);
		turn = (turn + 1) % players.len();

		if players.iter().any(|p| p.score >= 1000) {
			break;
		}
	}

	let loser = players.iter().min_by(|a, b| a.score.cmp(&b.score)).unwrap();

	println!("Answer: {}", loser.score * die.rolls);
}
