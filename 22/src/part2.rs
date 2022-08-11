use rayon::prelude::*;
use regex::Regex;
use std::fs;
use std::time::Instant;

#[derive(PartialEq, Eq)]
enum Action {
	On,
	Off,
}

struct Area {
	action: Action,
	x: (i64, i64),
	y: (i64, i64),
	z: (i64, i64),
}

impl Area {
	fn contains(&self, x: i64, y: i64, z: i64) -> bool {
		self.x.0 <= x
			&& x <= self.x.1
			&& self.y.0 <= y
			&& y <= self.y.1
			&& self.z.0 <= z
			&& z <= self.z.1
	}
}

fn parse_command(input: &str) -> Area {
	let re =
		Regex::new(r"^(on|off) x=(-?\d+)\.\.(-?\d+),y=(-?\d+)\.\.(-?\d+),z=(-?\d+)\.\.(-?\d+)$")
			.unwrap();

	let cap = re.captures(input).unwrap();

	let nums = cap
		.iter()
		.skip(2)
		.filter_map(|x| x.unwrap().as_str().parse::<i64>().ok())
		.collect::<Vec<_>>();

	Area {
		action: match &cap[1] {
			"on" => Action::On,
			"off" => Action::Off,
			_ => panic!("Unknown command"),
		},
		x: (nums[0], nums[1]),
		y: (nums[2], nums[3]),
		z: (nums[4], nums[5]),
	}
}

fn main() {
	let now = Instant::now();
	let contents = fs::read_to_string("input.txt").unwrap();

	let areas = contents.lines().map(parse_command).collect::<Vec<_>>();

	let collect_borders = |v: &Vec<Area>, f: fn(&Area) -> [i64; 2]| {
		let mut new: Vec<i64> = v.iter().flat_map(f).collect();
		new.sort();
		new.dedup();
		new
	};

	let xs = collect_borders(&areas, |a| [a.x.0, a.x.1 + 1]);
	let ys = collect_borders(&areas, |a| [a.y.0, a.y.1 + 1]);
	let zs = collect_borders(&areas, |a| [a.z.0, a.z.1 + 1]);

	let areas = areas.iter().rev().collect::<Vec<_>>();

	let sum: i64 = xs
		.par_iter()
		.zip(xs.par_iter().skip(1))
		.map(|(x1, x2)| {
			let mut sum = 0;
			for (y1, y2) in ys.iter().zip(ys.iter().skip(1)) {
				for (z1, z2) in zs.iter().zip(zs.iter().skip(1)) {
					for area in areas.iter() {
						if area.contains(*x1, *y1, *z1) {
							sum += match area.action {
								Action::On => (x2 - x1) * (y2 - y1) * (z2 - z1),
								Action::Off => 0,
							};
							break;
						}
					}
				}
			}
			sum
		})
		.sum();

	let elapsed = now.elapsed();
	println!("elapsed: {:.2?}", elapsed);

	println!("{}", sum);
}
