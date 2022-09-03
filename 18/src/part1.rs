#![allow(clippy::pedantic)]
use std::fmt::Write as _;
use std::fs;

fn explode(line: &mut Vec<(i32, i32)>) -> bool {
	let explode_pos = line.windows(2).position(|x| x[0].0 == 5 && x[1].0 == 5);

	if explode_pos.is_none() {
		return false;
	}

	// print!("before explode: ");
	// print_line(line);

	let explode_pos = explode_pos.unwrap();

	if explode_pos > 0 {
		line[explode_pos - 1].1 += line[explode_pos].1;
	}
	if explode_pos < line.len() - 2 {
		line[explode_pos + 2].1 += line[explode_pos + 1].1;
	}
	let (d, _) = line.remove(explode_pos);
	line[explode_pos] = (d - 1, 0);

	// print!("after explode: ");
	// print_line(line);

	explode(line);
	true
}

fn split(line: &mut Vec<(i32, i32)>) -> bool {
	let split_pos = line.iter().position(|(_, n)| *n >= 10);

	if split_pos.is_none() {
		return false;
	}

	// print!("before split: ");
	// print_line(line);

	let split_pos = split_pos.unwrap();

	let (depth, num) = line[split_pos];

	line[split_pos] = (depth + 1, num / 2);
	line.insert(
		split_pos + 1,
		(depth + 1, ((num as f32) / 2.0).ceil() as i32),
	);

	// print!("after split: ");
	// print_line(line);

	true
}

fn print_line(line: &[(i32, i32)]) {
	let mut out = String::new();
	for (d, n) in line.iter() {
		let _ = write!(out, "{d}:{n} ");
	}

	println!("{out}\n");
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

fn main() {
	let contents = fs::read_to_string("input.txt").unwrap();
	let lines = contents.lines();

	let mut num_lines = lines.map(|l| {
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
	});

	let mut result = num_lines.next().unwrap();

	for cur_line in num_lines {
		result = result
			.iter()
			.chain(cur_line.iter())
			.map(|(d, n)| (d + 1, *n))
			.collect();

		loop {
			let exploded = explode(&mut result);

			let split = split(&mut result);

			if !exploded && !split {
				break;
			}
		}
	}

	print_line(&result);

	println!("{}", magnitude(&mut result));
}
