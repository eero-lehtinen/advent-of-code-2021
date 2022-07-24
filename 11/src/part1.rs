use std::fs;

fn parse_num_line(line: &str) -> Vec<i32> {
	line.chars()
		.flat_map(|c| c.to_digit(10))
		.map(|u| u as i32)
		.collect()
}

fn simulate(mut nums: Vec<Vec<(bool, i32)>>) -> i32 {
	fn safe_increment(nums: &mut [Vec<(bool, i32)>], x: i32, y: i32) {
		if x < 0 || y < 0 || x > 9 || y > 9 {
			return;
		}
		nums[x as usize][y as usize].1 += 1;
	}

	let mut flashes = 0;

	for _ in 0..100 {
		for line in &mut nums {
			for n in line {
				if n.1 > 9 {
					n.1 = 0
				}
				n.0 = false;
				n.1 += 1;
			}
		}

		let mut done = false;
		while !done {
			done = true;
			for x in 0..10 {
				for y in 0..10 {
					let val = &mut nums[x as usize][y as usize];
					if !val.0 && val.1 > 9 {
						val.0 = true;
						safe_increment(&mut nums, x - 1, y - 1);
						safe_increment(&mut nums, x - 1, y);
						safe_increment(&mut nums, x - 1, y + 1);
						safe_increment(&mut nums, x, y + 1);
						safe_increment(&mut nums, x + 1, y + 1);
						safe_increment(&mut nums, x + 1, y);
						safe_increment(&mut nums, x + 1, y - 1);
						safe_increment(&mut nums, x, y - 1);
						flashes += 1;
						done = false;
					}
				}
			}
		}
	}

	flashes
}

fn main() {
	let contents = fs::read_to_string("input.txt").unwrap();

	let nums: Vec<Vec<(bool, i32)>> = contents
		.lines()
		.map(parse_num_line)
		.map(|l| l.iter().map(|n| (false, *n)).collect())
		.collect();

	println!("{:?}", simulate(nums));
}
