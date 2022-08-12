use std::fs;

fn explode(line: &mut Vec<(i32, i32)>) -> bool {
	let explode_pos = line.windows(2).position(|x| x[0].0 == 5 && x[1].0 == 5);

	if explode_pos.is_none() {
		return false;
	}

	let explode_pos = explode_pos.unwrap();

	if explode_pos > 0 {
		line[explode_pos - 1].1 += line[explode_pos].1;
	}
	if explode_pos < line.len() - 2 {
		line[explode_pos + 2].1 += line[explode_pos + 1].1;
	}
	let (d, _) = line.remove(explode_pos);
	line[explode_pos] = (d - 1, 0);

	explode(line);
	true
}

fn split(line: &mut Vec<(i32, i32)>) -> bool {
	let split_pos = line.iter().position(|(_, n)| *n >= 10);

	if split_pos.is_none() {
		return false;
	}

	let split_pos = split_pos.unwrap();

	let (depth, num) = line[split_pos];

	line[split_pos] = (depth + 1, num / 2);
	line.insert(
		split_pos + 1,
		(depth + 1, ((num as f32) / 2.0).ceil() as i32),
	);

	true
}

fn magnitude(line: &mut Vec<(i32, i32)>) -> i32 {
	while line.len() > 1 {
		let mut max_depth_idx = 0;
		let mut max_depth = 0;
		for i in 0..(line.len() - 1) {
			if line[i].0 == line[i + 1].0 && line[i].0 > max_depth {
				max_depth = line[i].0;
				max_depth_idx = i;
			}
		}
		let left = line[max_depth_idx];
		let right = line[max_depth_idx + 1];

		line.remove(max_depth_idx);
		line[max_depth_idx] = (max_depth - 1, 3 * left.1 + 2 * right.1);
	}

	line.first().unwrap().1
}

fn addition(left: &[(i32, i32)], right: &[(i32, i32)]) -> i32 {
	let mut line = left
		.iter()
		.chain(right.iter())
		.map(|(d, n)| (d + 1, *n))
		.collect();

	loop {
		let exploded = explode(&mut line);

		let split = split(&mut line);

		if !exploded && !split {
			break;
		}
	}

	magnitude(&mut line)
}

fn main() {
	let contents = fs::read_to_string("input.txt").unwrap();
	let lines = contents.lines();

	let lines = lines
		.map(|l| {
			let mut depth = 0;
			let mut nums = vec![];
			for c in l.chars() {
				if let Some(n) = c.to_digit(10) {
					nums.push((depth, i32::try_from(n).unwrap()));
				} else if c == '[' {
					depth += 1;
				} else if c == ']' {
					depth -= 1;
				}
			}
			nums
		})
		.collect::<Vec<_>>();

	let mut combinations: Vec<(usize, usize)> = vec![];
	for x in 0..lines.len() {
		for y in 0..lines.len() {
			if x != y {
				combinations.push((x, y));
			}
		}
	}

	let mut largest_magnitude = 0;
	for (a, b) in combinations {
		let magnitude = addition(&lines[a], &lines[b]);
		if magnitude > largest_magnitude {
			largest_magnitude = magnitude;
		}
	}

	println!("{}", largest_magnitude);
}
