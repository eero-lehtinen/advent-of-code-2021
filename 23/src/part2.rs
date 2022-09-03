#![allow(clippy::too_many_lines)]
use std::cmp::{Eq, Ord, Ordering, PartialEq, PartialOrd};
use std::collections::{BinaryHeap, HashMap};
use std::fs;

#[derive(PartialEq, Eq, Hash, Clone)]
struct Move {
	from: usize,
	to: usize,
	cost: i64,
}

#[derive(PartialEq, Eq, Default, Copy, Clone, Hash)]
enum Amphipod {
	A = 0,
	B = 1,
	C = 2,
	D = 3,
	#[default]
	Empty = 4,
}

type State = [Amphipod; HALLWAY_SIZE + ROOM_COUNT * ROOM_SIZE];

const ROOM_COUNT: usize = 4;
const ROOM_SIZE: usize = 4;
const HALLWAY_SIZE: usize = 11;

fn amphipod_to_str(a: &Amphipod) -> &str {
	match a {
		Amphipod::A => "A",
		Amphipod::B => "B",
		Amphipod::C => "C",
		Amphipod::D => "D",
		Amphipod::Empty => ".",
	}
}

fn char_to_amphipod(c: char) -> Amphipod {
	match c {
		'A' => Amphipod::A,
		'B' => Amphipod::B,
		'C' => Amphipod::C,
		'D' => Amphipod::D,
		'.' => Amphipod::Empty,
		_ => panic!("invalid char"),
	}
}

fn print_state(state: &State) {
	let hallway = state[ROOM_COUNT * ROOM_SIZE..]
		.iter()
		.map(amphipod_to_str)
		.collect::<String>();

	println!("#{}#", hallway);

	for i in 0..ROOM_SIZE {
		let mut slice = String::new();
		for j in 0..ROOM_COUNT {
			slice.push_str(amphipod_to_str(&state[j * ROOM_SIZE + i]));
			slice.push('#');
		}
		println!("###{}##", slice);
	}
}

#[derive(PartialEq, Eq, Hash, Clone)]
struct HeapItem {
	mv: Move,
	cost: i64,
	state: State,
}

impl PartialOrd for HeapItem {
	fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
		Some(self.cmp(other))
	}
}

impl Ord for HeapItem {
	fn cmp(&self, other: &Self) -> Ordering {
		self.cost.cmp(&other.cost).reverse()
	}
}

///
fn distance(mut from: usize, mut to: usize) -> i64 {
	if from >= ROOM_COUNT * ROOM_SIZE {
		std::mem::swap(&mut from, &mut to);
	}
	assert!(
		from < ROOM_COUNT * ROOM_SIZE,
		"from and to need to go from room to hallway, not hallway to hallway or room to room"
	);

	// Currently from is room and to is hallway

	let room = from / ROOM_SIZE;
	let room_pos = from % ROOM_SIZE;

	let hallway_pos = to - ROOM_COUNT * ROOM_SIZE;
	let room_entrance_pos = 2 + room * 2;

	room_pos as i64 + 1 + hallway_pos.abs_diff(room_entrance_pos) as i64
}

fn get_possible_moves(from_state: &State) -> Vec<(Move, State)> {
	let get_to_hallway_moves = |i: usize| -> Vec<(Move, State)> {
		let amphipod = from_state[i];
		if amphipod == Amphipod::Empty {
			return vec![];
		}

		// Check if room entrance is blocked and we can't move
		if i % ROOM_SIZE != 0 {
			let mut above_check_i = i - 1;
			loop {
				if from_state[above_check_i] != Amphipod::Empty {
					return vec![];
				}
				if above_check_i % ROOM_SIZE == 0 {
					break;
				}
				above_check_i -= 1;
			}
		}

		let room = i / ROOM_SIZE;

		// Check if we are already in the bottom of the correct room
		if from_state[i..room * ROOM_SIZE + ROOM_SIZE]
			.iter()
			.all(|&a| a as usize == room)
		{
			return vec![];
		}

		let mut valid_moves = vec![];
		let hallway_spots = &from_state[ROOM_COUNT * ROOM_SIZE..];

		let [mut left_pos, mut right_pos] = [2 + 2 * room; 2];

		while hallway_spots[left_pos] == Amphipod::Empty {
			if left_pos % 2 != 0 || left_pos < 2 {
				valid_moves.push(ROOM_COUNT * ROOM_SIZE + left_pos);
			}
			if left_pos == 0 {
				break;
			}
			left_pos -= 1;
		}

		while hallway_spots[right_pos] == Amphipod::Empty {
			if right_pos % 2 != 0 || right_pos >= HALLWAY_SIZE - 2 {
				valid_moves.push(ROOM_COUNT * ROOM_SIZE + right_pos);
			}
			if right_pos == hallway_spots.len() - 1 {
				break;
			}
			right_pos += 1;
		}

		let mut new_state = *from_state;
		new_state[i] = Amphipod::Empty;

		valid_moves
			.iter()
			.map(|&j| {
				let mut s = new_state;
				s[j] = amphipod;
				(
					Move {
						from: i,
						to: j,
						cost: distance(i, j) * 10i64.pow(amphipod as u32),
					},
					s,
				)
			})
			.collect()
	};

	let get_to_room_moves = |i: usize| -> Vec<(Move, State)> {
		let amphipod = from_state[i];
		if amphipod == Amphipod::Empty {
			return vec![];
		}

		let target_room = amphipod as usize;

		let mut lowest_valid_move = None;

		for (j, &amp) in from_state
			.iter()
			.enumerate()
			.skip(target_room * ROOM_SIZE)
			.take(ROOM_SIZE)
		{
			if amp == Amphipod::Empty {
				lowest_valid_move = Some(j);
			}
			// Rule 2
			else if amp != amphipod {
				return vec![];
			}
		}

		let hallway_pos = i - ROOM_COUNT * ROOM_SIZE;
		let room_entrance_pos = 2 + target_room * 2;

		let (check_from, check_to) = if room_entrance_pos < hallway_pos {
			(room_entrance_pos, hallway_pos - 1)
		} else {
			(hallway_pos + 1, room_entrance_pos)
		};

		if from_state
			.iter()
			.skip(ROOM_COUNT * ROOM_SIZE)
			.skip(check_from)
			.take(check_to - check_from)
			.any(|x| !matches!(x, Amphipod::Empty))
		{
			return vec![];
		}

		match lowest_valid_move {
			None => vec![],
			Some(j) => {
				let mut s = *from_state;
				s[i] = Amphipod::Empty;
				s[j] = amphipod;
				vec![(
					Move {
						from: i,
						to: j,
						cost: distance(i, j) * 10i64.pow(amphipod as u32),
					},
					s,
				)]
			}
		}
	};

	let mut moves = vec![];

	for (i, _) in from_state.iter().enumerate() {
		// From room to hallway
		if i < ROOM_COUNT * ROOM_SIZE {
			moves.append(&mut get_to_hallway_moves(i));
		} else {
			moves.append(&mut get_to_room_moves(i));
		}
	}

	moves
}

fn amphipod_search(start: State) -> i64 {
	let finished_state = read_state(
		"#############
#...........#
###A#B#C#D###
  #A#B#C#D#
  #A#B#C#D#
  #A#B#C#D#
  #########",
	);

	// 	let check_state = read_state(
	// 		"#############
	// #...B.D.A...#
	// ###.#.#C#.###
	// ###A#B#C#D###",
	// 	);

	let mut heap = BinaryHeap::new();
	heap.push(HeapItem {
		cost: 0,
		mv: Move {
			from: 0,
			to: 0,
			cost: 0,
		},
		state: start,
	});

	let mut last: HashMap<HeapItem, HeapItem> = HashMap::new();

	// println!("init heap moves:");
	// heap.iter().for_each(|m| print_state(&m.state));
	// println!();

	// let mut count = 0;
	let mut finish = None;

	while !heap.is_empty() {
		// count += 1;
		let item = heap.pop().unwrap();

		// if count % 1000 == 0 {
		// 	println!("cost: {}", item.cost);
		// 	print_state(&item.state);
		// 	println!();
		// }

		// if item.state == check_state {
		// 	println!("test:");
		// 	print_state(&item.state);
		// }

		// println!("move:");
		// print_state(&item.state);
		// println!();

		if item.state == finished_state {
			finish = Some(item);
			break;
		}

		let possible_moves = get_possible_moves(&item.state)
			.into_iter()
			.map(|res| HeapItem {
				cost: item.cost + res.0.cost,
				mv: res.0,
				state: res.1,
			});

		let possible_moves = possible_moves
			.into_iter()
			.filter(|x| !last.contains_key(x))
			.collect::<Vec<_>>();

		possible_moves.iter().cloned().for_each(|x| {
			last.insert(x, item.clone());
		});

		// println!("possible moves:");
		// possible_moves.iter().for_each(|m| print_state(&m.state));
		// println!();

		heap.append(&mut BinaryHeap::from(possible_moves));
	}

	let finish = finish.unwrap();

	let mut item: &HeapItem = &finish;

	println!("FINISH:");
	loop {
		let item_opt = last.get(item);
		item = match item_opt {
			None => break,
			Some(i) => {
				println!("cost: {}", i.cost);
				print_state(&i.state);
				i
			}
		}
	}

	finish.cost
}

fn read_state(input: &str) -> State {
	let mut state: State = [Amphipod::Empty; 27];
	let mut lines = input.lines();
	lines.next();
	for (i, c) in lines
		.next()
		.unwrap()
		.chars()
		.skip(1)
		.take(HALLWAY_SIZE)
		.enumerate()
	{
		state[ROOM_COUNT * ROOM_SIZE + i] = char_to_amphipod(c);
	}
	for i in 0..ROOM_SIZE {
		for (j, c) in lines
			.next()
			.unwrap()
			.chars()
			.skip(3)
			.step_by(2)
			.take(ROOM_COUNT)
			.enumerate()
		{
			state[ROOM_SIZE * j + i] = char_to_amphipod(c);
		}
	}

	state
}

fn main() {
	let contents = fs::read_to_string("input2.txt").unwrap();

	let start = read_state(&contents[..]);
	println!("initial:");
	print_state(&start);

	let cost = amphipod_search(start);

	println!("cost: {cost}");
}
