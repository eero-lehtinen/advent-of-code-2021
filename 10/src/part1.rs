use std::fs;

fn closing_to_opening(char: char) -> char {
	match char {
		')' => '(',
		']' => '[',
		'}' => '{',
		'>' => '<',
		_ => panic!("Unexpected character"),
	}
}

fn close_char(open_chars: &mut Vec<char>, char: char) -> Option<i64> {
	let opening_char = closing_to_opening(char);

	match open_chars.last().map(|c| *c == opening_char) {
		Some(true) => {
			open_chars.pop();
			None
		}
		Some(false) => {
			let score = match char {
				')' => 3,
				']' => 57,
				'}' => 1197,
				'>' => 25137,
				_ => panic!("Unexpected character"),
			};

			Some(score)
		}
		_ => None,
	}
}

fn calc_error_score(line: &str) -> i64 {
	let mut open_chars = Vec::new();

	for char in line.chars() {
		if "([{<".chars().any(|c| c == char) {
			open_chars.push(char);
			continue;
		}

		if let Some(score) = close_char(&mut open_chars, char) {
			return score;
		}
	}

	0
}

fn main() {
	let contents = fs::read_to_string("input.txt").unwrap();

	let scores: Vec<i64> = contents.lines().map(calc_error_score).collect();
	let sum: i64 = scores.iter().sum();
	
	println!("{:?}", scores);
	println!("{:?}", sum);
}
