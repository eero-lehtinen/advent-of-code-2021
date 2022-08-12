use std::fs::read_to_string;

fn part1() {
	let contents = read_to_string("input.txt").unwrap();

	let mut gamma = 0u32;
	let mut epsilon = 0u32;

	let mut one_counts = [0; 32];
	let mut zero_counts = [0; 32];

	for line in contents.lines() {
		for (i, ch) in line.chars().enumerate() {
			let bit = ch.to_digit(10).unwrap();
			match bit {
				1 => one_counts[i] += 1,
				0 => zero_counts[i] += 1,
				_ => panic!("Invalid bit!"),
			}
		}
	}

	let mut length = 0usize;
	loop {
		length += 1;
		if one_counts[length] == 0 && zero_counts[length] == 0 {
			break;
		}
	}

	for i in 0..(length - 1) {
		if one_counts[i] > zero_counts[i] {
			gamma |= 1u32 << ((length - 1) - i);
		} else {
			epsilon |= 1u32 << ((length - 1) - i);
		}
	}

	println!("{:?}", gamma);
	println!("{:?}", epsilon);
	println!("{:?}", gamma * epsilon);
}

fn filter_nums(mut numbers: Vec<u32>, cmp: &dyn Fn(i32, i32) -> bool, bit_len: usize) -> u32 {
	for i in 0..bit_len {
		if numbers.len() == 1 {
			break;
		}

		let bit_pos = (bit_len - 1) - i;
		let mut one_count = 0;
		let mut zero_count = 0;

		for number in &numbers {
			if number >> bit_pos & 1 == 1 {
				one_count += 1;
			} else {
				zero_count += 1;
			}
		}

		let most_common_bit = if cmp(one_count, zero_count) { 1 } else { 0 };

		numbers.retain(|n| (n >> bit_pos & 1 == most_common_bit));
	}
	return *numbers.last().unwrap();
}

fn part2() {
	let contents = read_to_string("input.txt").unwrap();
	let mut numbers: Vec<u32> = Vec::new();

	let length = contents.lines().last().unwrap().len();
	for line in contents.lines() {
		numbers.push(u32::from_str_radix(line, 2).unwrap());
	}

	let oxygen = filter_nums(numbers.clone(), &|o, z| o >= z, length);
	let co2 = filter_nums(numbers.clone(), &|o, z| o < z, length);

	println!("{0:?} {0:b}", oxygen);
	println!("{0:?} {0:b}", co2);
	println!("{}", oxygen * co2);
}

fn main() {
	part1();
	part2();
}
