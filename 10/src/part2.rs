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

fn calc_autocomplete_score(line: &str) -> Option<i64> {
	let mut open_chars = Vec::new();

	for char in line.chars() {
		if "([{<".chars().any(|c| c == char) {
			open_chars.push(char);
			continue;
		}

		if let Some(_) = close_char(&mut open_chars, char) {
			return None; // Ignore corrupted
		}
	}

	let score = open_chars.iter().rev().fold(0, |acc, c| {
		acc * 5
			+ match c {
				'(' => 1,
				'[' => 2,
				'{' => 3,
				'<' => 4,
				_ => panic!("Unexpected character"),
			}
	});

	Some(score)
}

fn median(numbers: &Vec<i64>) -> i64 {
	let mut numbers = numbers.clone();
	numbers.sort_unstable();
	let mid = numbers.len() / 2;
	numbers[mid]
}

fn main() {
	let contents = fs::read_to_string("input.txt").unwrap();
	let scores: Vec<i64> = contents.lines().flat_map(calc_autocomplete_score).collect();

	let middle: i64 = median(&scores);

	println!("{:?}", scores);
	println!("{:?}", middle);
}
