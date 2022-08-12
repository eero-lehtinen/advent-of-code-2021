use bitvec::prelude::*;
use std::fs;

fn read_literal(bits: &BitSlice<u8, Msb0>) -> (u64, usize) {
	let mut bits = bits;
	let mut num_bits: BitVec<u8, Msb0> = BitVec::new();
	let mut skipped_bits = 0;
	loop {
		num_bits.extend(&bits[1..5]);
		let stop = !bits[0];
		bits = &bits[5..];
		skipped_bits += 5;
		if stop {
			break;
		}
	}
	(num_bits.load_be(), skipped_bits)
}

fn read_packet(bits: &BitSlice<u8, Msb0>) -> (u64, usize) {
	let mut bits = bits;
	let orig_bits_len = bits.len();

	let result;

	println!("bits: {bits}");

	let version: u8 = bits[..3].load_be();
	let type_id: u8 = bits[3..6].load_be();
	bits = &bits[6..];

	println!("version: {version}");
	println!("type id: {type_id}");

	match type_id {
		4 => {
			let (literal, read_len) = read_literal(bits);
			bits = &bits[read_len..];
			println!("literal: {}", literal);

			result = literal;
		}
		op => {
			let operate = |x: Vec<u64>| -> u64 {
				match op {
					0 => x.iter().sum(),
					1 => x.iter().product(),
					2 => *x.iter().min().unwrap(),
					3 => *x.iter().max().unwrap(),
					5 => u64::from(x[0] > x[1]),
					6 => u64::from(x[0] < x[1]),
					7 => u64::from(x[0] == x[1]),
					_ => panic!("Invalid operator"),
				}
			};

			let length_type_id = bits[0];
			bits = &bits[1..];
			println!("length_type_id: {}", length_type_id);

			let mut sub_results = Vec::new();

			if length_type_id {
				let sub_packet_len: usize = bits[..11].load_be();
				bits = &bits[11..];
				for _ in 0..sub_packet_len {
					let (sub_res, read_len) = read_packet(bits);
					sub_results.push(sub_res);
					bits = &bits[read_len..];
				}
			} else {
				let sub_packet_bit_len: usize = bits[..15].load_be();
				bits = &bits[15..];
				println!("sub_packet_bit_len: {}", sub_packet_bit_len);
				let mut processed_len: usize = 0;

				while processed_len < sub_packet_bit_len {
					let (sub_res, read_len) = read_packet(bits);
					sub_results.push(sub_res);
					processed_len += read_len;
					bits = &bits[read_len..];
				}
			}

			result = operate(sub_results);
		}
	};

	let read_len = orig_bits_len - bits.len();
	println!("read bits len: {read_len}");
	(result, read_len)
}

fn main() {
	let contents = fs::read_to_string("input.txt").unwrap();

	let nums = hex::decode(contents).unwrap();

	let bits = nums.view_bits::<Msb0>();

	let (value, _read_len) = read_packet(bits);

	println!("RESULT: {}", value);
}
