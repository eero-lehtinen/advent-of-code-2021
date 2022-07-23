use std::{collections::HashMap, fs};

fn main() {
	let contents = fs::read_to_string("input.txt").unwrap();

	let mut lines = contents.lines();

	let mut state = lines.next().unwrap().to_owned().as_bytes().to_vec();

	let transitions = lines
		.skip(1)
		.map(|l| ((l.as_bytes()[0], l.as_bytes()[1]), l.as_bytes()[6]))
		.collect::<Vec<_>>();

	for _ in 0..10 {
		let mut new_state: Vec<u8> = Vec::with_capacity(state.len());
		for (c1, c2) in state.iter().zip(state.iter().skip(1)) {
			let t = transitions.iter().find(|t| t.0 .0 == *c1 && t.0 .1 == *c2);
			match t {
				Some(t) => new_state.extend([c1, &t.1]),
				_ => new_state.push(*c1),
			};
		}
		new_state.push(*state.last().unwrap());
		state = new_state;
	}

	let mut counts = HashMap::new();
	for c in state {
		*counts.entry(c).or_insert(0) += 1;
	}

	let max = counts.iter().max_by_key(|&(_, count)| count).unwrap();
	let min = counts.iter().min_by_key(|&(_, count)| count).unwrap();

	println!("{}", max.1 - min.1);
}
