use std::fs;

#[derive(PartialEq, Eq, Clone, Debug, Ord, PartialOrd)]
struct Coord {
	x: i64,
	y: i64,
	z: i64,
}

#[derive(Clone, Debug)]
struct Rot(i64, i64, i64);

fn parse_coord_line(line: &str) -> Coord {
	let mut parts = line.split(',').filter_map(|s| s.parse::<i64>().ok());
	Coord {
		x: parts.next().unwrap(),
		y: parts.next().unwrap(),
		z: parts.next().unwrap(),
	}
}

fn manhattan_dist(a: &Coord, b: &Coord) -> i64 {
	(a.x - b.x).abs() + (a.y - b.y).abs() + (a.z - b.z).abs()
}

fn rotate(c: &Coord, rot: u32) -> Coord {
	let (x, y, z) = match rot {
		// positive x
		0 => (c.x, c.y, c.z),
		1 => (c.x, -c.z, c.y),
		2 => (c.x, -c.y, -c.z),
		3 => (c.x, c.z, -c.y),
		// negative x
		4 => (-c.x, -c.y, c.z),
		5 => (-c.x, c.z, c.y),
		6 => (-c.x, c.y, -c.z),
		7 => (-c.x, -c.z, -c.y),
		// positive y
		8 => (c.y, c.z, c.x),
		9 => (c.y, -c.x, c.z),
		10 => (c.y, -c.z, -c.x),
		11 => (c.y, c.x, -c.z),
		// #negative y
		12 => (-c.y, -c.z, c.x),
		13 => (-c.y, c.x, c.z),
		14 => (-c.y, c.z, -c.x),
		15 => (-c.y, -c.x, -c.z),
		// #positive z
		16 => (c.z, c.x, c.y),
		17 => (c.z, -c.y, c.x),
		18 => (c.z, -c.x, -c.y),
		19 => (c.z, c.y, -c.x),
		// #negative z
		20 => (-c.z, -c.x, c.y),
		21 => (-c.z, c.y, c.x),
		22 => (-c.z, c.x, -c.y),
		23 => (-c.z, -c.y, -c.x),
		_ => panic!("Unsupported rotation"),
	};
	Coord { x, y, z }
}

fn apply_offset(coord: &Coord, offset: &Coord) -> Coord {
	Coord {
		x: coord.x - offset.x,
		y: coord.y - offset.y,
		z: coord.z - offset.z,
	}
}

fn solve_scanner_coord_rot(
	origin_detections: &[Coord],
	target_detections: &[Coord],
	matches: &[(usize, usize, i64)],
) -> Option<(Coord, u32)> {
	let origin_d = &origin_detections[matches[0].0];
	let target_d = &target_detections[matches[0].1];

	for rot in 0..24 {
		let target_rot_d = rotate(target_d, rot);
		let offset = Coord {
			x: target_rot_d.x - origin_d.x,
			y: target_rot_d.y - origin_d.y,
			z: target_rot_d.z - origin_d.z,
		};

		let mut match_count = 0;
		for m in matches.iter().skip(1) {
			let d1 = &origin_detections[m.0];
			let mut d2 = target_detections[m.1].to_owned();
			d2 = rotate(&d2, rot);
			d2 = apply_offset(&d2, &offset);

			if *d1 == d2 {
				match_count += 1;
			}
		}

		if match_count >= 11 {
			return Some((offset, rot));
		}
	}

	None
}

fn calc_rel_dist(points: &[Coord]) -> Vec<Vec<(usize, i64)>> {
	let mut ret: Vec<Vec<(usize, i64)>> = Vec::new();
	for det in points.iter() {
		let x: Vec<(usize, i64)> = points
			.iter()
			.enumerate()
			.map(|(idx, d)| (idx, manhattan_dist(det, d)))
			.collect();
		ret.push(x);
	}
	ret
}

fn main() {
	let contents = fs::read_to_string("input.txt").unwrap();
	let scanner_contents = contents.split("\n\n");

	let detections: Vec<Vec<Coord>> = scanner_contents
		.map(|c| c.lines().skip(1).map(parse_coord_line).collect())
		.collect();

	let mut detection_rel_dist: Vec<Vec<Vec<(usize, i64)>>> = vec![];
	for det in detections.iter() {
		detection_rel_dist.push(calc_rel_dist(det));
	}

	let mut solved_points = detections[0].clone();
	let mut solved_rel_dist = detection_rel_dist[0].clone();

	let mut solved = vec![false; detections.len()];
	solved[0] = true;

	while solved.iter().any(|a| !a) {
		for i in 0..solved_rel_dist.len() {
			// k is the one we want to solve
			for k in 0..detection_rel_dist.len() {
				if solved[k] {
					continue;
				}
				for l in 0..detection_rel_dist[k].len() {
					let matches = solved_rel_dist[i]
						.iter()
						.filter_map(|a| {
							detection_rel_dist[k][l]
								.iter()
								.find(|b| a.1 == b.1)
								.map(|b| (a.0, b.0, b.1))
						})
						.collect::<Vec<_>>();

					if matches.len() >= 12 {
						if let Some((offset, rot)) =
							solve_scanner_coord_rot(&solved_points, &detections[k], &matches)
						{
							println!("solved {k}");
							solved_points.append(
								&mut detections[k]
									.iter()
									.map(|d| rotate(d, rot))
									.map(|d| apply_offset(&d, &offset))
									.collect(),
							);

							solved_rel_dist = calc_rel_dist(&solved_points);

							solved[k] = true;
							break;
						}
					}
				}
			}
		}
	}

	println!("done");

	solved_points.sort_unstable();
	solved_points.dedup();

	println!("len: {:?}", solved_points.len());
}
