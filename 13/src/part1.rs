use std::fs;

#[derive(Debug, Clone, Ord, Eq, PartialEq, PartialOrd)]
struct Point {
	x: i32,
	y: i32,
}

fn parse_point(input: &str) -> Option<Point> {
	let mut iter = input.split(',').filter_map(|s| s.parse().ok());
	match (iter.next(), iter.next()) {
		(Some(x), Some(y)) => Some(Point { x, y }),
		_ => None,
	}
}

enum Direction {
	Vertical,
	Horizontal,
}

fn fold_points(points: &mut Vec<Point>, dir: &Direction, at: i32) {
	let f = |val| if val > at { at - (val - at) } else { val };

	for p in points.iter_mut() {
		match dir {
			Direction::Vertical => p.y = f(p.y),
			Direction::Horizontal => p.x = f(p.x),
		}
	}

	points.sort();
	points.dedup();
}

fn main() {
	let contents = fs::read_to_string("input.txt").unwrap();

	let (point_contents, fold_contents) = contents.split_at(contents.find("\n\n").unwrap());

	let fold_commands: Vec<_> = fold_contents
		.lines()
		.skip(2)
		.map(|l| (l.chars().nth(11).unwrap(), l[13..].parse::<i32>().unwrap()))
		.map(|(a, b)| {
			(
				match a {
					'x' => Direction::Horizontal,
					'y' => Direction::Vertical,
					_ => panic!("invalid character"),
				},
				b,
			)
		})
		.collect();

	let mut points: Vec<Point> = point_contents.lines().filter_map(parse_point).collect();

	let (dir, at) = &fold_commands[0];
	fold_points(&mut points, dir, *at);

	println!("{:?}", points.len());
}
