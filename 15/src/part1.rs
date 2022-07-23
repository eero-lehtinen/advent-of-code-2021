use std::cmp::*;
use std::collections::HashMap;
use std::{collections::BinaryHeap, fs};

fn parse_num_line(line: &str) -> Vec<i32> {
	line.chars()
		.flat_map(|c| c.to_digit(10).map(|x| x as i32))
		.collect()
}

#[derive(PartialEq, Eq)]
struct HeapItem {
	pos: (usize, usize),
	f_score: i32,
}

impl PartialOrd for HeapItem {
	fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
		Some(self.cmp(other))
	}
}

impl Ord for HeapItem {
	fn cmp(&self, other: &HeapItem) -> Ordering {
		self.f_score.cmp(&other.f_score).reverse()
	}
}

fn manhattan_dist(a: (usize, usize), b: (usize, usize)) -> i32 {
	(if a.0 >= b.0 { a.0 - b.0 } else { b.0 - a.0 } as i32)
		+ (if a.1 >= b.1 { a.1 - b.1 } else { b.1 - a.1 } as i32)
}

fn a_star(nodes: Vec<Vec<i32>>) -> Vec<(usize, usize)> {
	let start_pos = (0, 0);
	let end_pos = (nodes.first().unwrap().len() - 1, nodes.len() - 1);

	let h = |pos| manhattan_dist(pos, end_pos);

	let mut open_set = BinaryHeap::new();
	open_set.push(HeapItem {
		pos: start_pos,
		f_score: h(start_pos),
	});

	let mut came_from = HashMap::new();
	let mut g_scores = HashMap::new();
	g_scores.insert(start_pos, 0);

	let mut f_scores = HashMap::new();
	f_scores.insert(start_pos, h(start_pos));

	let get_neighbors = |(x, y): (usize, usize)| -> Vec<(usize, usize)> {
		let mut neighbors = Vec::new();
		if x > 0 {
			neighbors.push((x - 1, y));
		}
		if y > 0 {
			neighbors.push((x, y - 1));
		}
		if x < nodes.first().unwrap().len() - 1 {
			neighbors.push((x + 1, y));
		}
		if y < nodes.len() - 1 {
			neighbors.push((x, y + 1));
		}
		neighbors
	};

	while !open_set.is_empty() {
		let HeapItem { pos, f_score: _ } = open_set.pop().unwrap();

		if pos == end_pos {
			let mut path = Vec::from([pos]);
			let mut cur_pos = pos;
			while let Some(next_pos) = came_from.get(&cur_pos) {
				path.push(*next_pos);
				cur_pos = *next_pos;
			}
			path.reverse();
			return path;
		}

		for neigh_pos in get_neighbors(pos) {
			let tentative_g_score = g_scores.get(&pos).unwrap().to_owned() + nodes[pos.1][pos.0];
			if tentative_g_score < *g_scores.entry(neigh_pos).or_insert(i32::MAX) {
				came_from.insert(neigh_pos, pos);
				g_scores.insert(neigh_pos, tentative_g_score);
				let f_score = tentative_g_score + h(neigh_pos);
				f_scores.insert(neigh_pos, f_score);

				if open_set.iter().find(|x| x.pos == neigh_pos).is_none() {
					open_set.push(HeapItem {
						pos: neigh_pos,
						f_score,
					});
				}
			}
		}
	}
	panic!("path not found");
}

fn main() {
	let contents = fs::read_to_string("input.txt").unwrap();

	let nodes = contents.lines().map(parse_num_line).collect::<Vec<_>>();

	let path = a_star(nodes.clone());

	let mut path_display = String::new();

	for (y, line) in nodes.iter().enumerate() {
		for (x, node) in line.iter().enumerate() {
			if path.contains(&(x, y)) {
				path_display.push_str(&format!("\x1b[94m{node}\x1b[0m"));
			} else {
				path_display.push_str(&node.to_string());
			}
		}
		path_display.push_str("\n");
	}

	println!("{path_display}");

	let cost = path
		.iter()
		.skip(1)
		.fold(0, |acc, (x, y)| acc + nodes[*y][*x]);

	println!("cost: {:?}", cost);
}
