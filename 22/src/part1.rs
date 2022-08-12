use std::fs;

use regex::Regex;

enum Action {
	On,
	Off,
}

struct Command {
	action: Action,
	ranges: [(i64, i64); 3],
}

fn parse_command(input: &str) -> Command {
	let re =
		Regex::new(r"^(on|off) x=(-?\d+)\.\.(-?\d+),y=(-?\d+)\.\.(-?\d+),z=(-?\d+)\.\.(-?\d+)$")
			.unwrap();

	let cap = re.captures(input).unwrap();

	let nums = cap
		.iter()
		.skip(2)
		.filter_map(|x| x.unwrap().as_str().parse::<i64>().ok())
		.collect::<Vec<_>>();

	Command {
		action: match &cap[1] {
			"on" => Action::On,
			"off" => Action::Off,
			_ => panic!("Unknown command"),
		},
		ranges: [(nums[0], nums[1]), (nums[2], nums[3]), (nums[4], nums[5])],
	}
}

fn main() {
	let contents = fs::read_to_string("input.txt").unwrap();

	let commands = contents.lines().map(parse_command);

	let mut cubes = vec![[[false; 101]; 101]; 101].into_boxed_slice();

	for command in commands {
		let target_state = match command.action {
			Action::On => true,
			Action::Off => false,
		};

		for x in command.ranges[0].0..=command.ranges[0].1 {
			let x = x + 50;
			if !(0..=100).contains(&x) {
				break;
			}

			for y in command.ranges[1].0..=command.ranges[1].1 {
				let y = y + 50;
				if !(0..=100).contains(&y) {
					break;
				}
				for z in command.ranges[2].0..=command.ranges[2].1 {
					let z = z + 50;
					if !(0..=100).contains(&z) {
						break;
					}

					cubes[usize::try_from(z).unwrap()][usize::try_from(y).unwrap()]
						[usize::try_from(x).unwrap()] = target_state;
				}
			}
		}
	}

	let lit: i64 = cubes
		.iter()
		.flatten()
		.flatten()
		.map(|b| if *b { 1 } else { 0 })
		.sum();

	println!("{lit}");
}
