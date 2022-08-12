use std::{collections::HashMap, fs};

fn main() {
	let contents = fs::read_to_string("input.txt").unwrap();

	let mut lines = contents.lines();

	let state = lines.next().unwrap().to_owned().chars().collect::<Vec<_>>();

	let transitions: HashMap<(char, char), char> = lines
		.skip(1)
		.map(|l| {
			(
				(l.chars().next().unwrap(), l.chars().nth(1).unwrap()),
				l.chars().nth(6).unwrap(),
			)
		})
		.collect();

	let mut pairs: HashMap<(char, char), i64> = HashMap::with_capacity(state.len());
	for w in state.windows(2) {
		*pairs.entry((w[0], w[1])).or_insert(0) += 1;
	}

	let mut char_counts = HashMap::new();
	for c in state {
		*char_counts.entry(c).or_insert(0) += 1;
	}

	for _ in 0..40 {
		for (pair, count) in pairs.clone() {
			if let Some(t) = transitions.get(&pair) {
				*pairs.entry(pair).or_insert(0) -= count;
				*pairs.entry((pair.0, *t)).or_insert(0) += count;
				*pairs.entry((*t, pair.1)).or_insert(0) += count;
				*char_counts.entry(*t).or_insert(0) += count;
			}
		}
	}

	let max = char_counts.iter().max_by_key(|&(_, count)| count).unwrap();
	let min = char_counts.iter().min_by_key(|&(_, count)| count).unwrap();

	println!("max: {:?}, min: {:?}, res: {}", max, min, max.1 - min.1);
}
