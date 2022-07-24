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

	println!("bits: {bits}");

	let version: u8 = bits[..3].load_be();
	let mut version_sum = version as u64;
	let type_id: u8 = bits[3..6].load_be();
	bits = &bits[6..];

	println!("version: {version}");
	println!("type id: {type_id}");

	match type_id {
		4 => {
			let (literal, read_len) = read_literal(bits);
			bits = &bits[read_len..];
			println!("literal: {}", literal);
		}
		_op => {
			let length_type_id = bits[0];
			bits = &bits[1..];
			println!("length_type_id: {}", length_type_id);
			match length_type_id {
				false => {
					let sub_packet_bit_len: usize = bits[..15].load_be();
					bits = &bits[15..];
					println!("sub_packet_bit_len: {}", sub_packet_bit_len);
					let mut processed_len: usize = 0;
					while processed_len < sub_packet_bit_len {
						let (v_sum, read_len) = read_packet(bits);
						version_sum += v_sum;
						processed_len += read_len;
						bits = &bits[read_len..]
					}
				}
				true => {
					let sub_packet_len: u32 = bits[..11].load_be();
					bits = &bits[11..];
					for _ in 0..sub_packet_len {
						let (v_sum, read_len) = read_packet(bits);
						version_sum += v_sum;
						bits = &bits[read_len..]
					}
				}
			}
		}
	};

	let read_len = orig_bits_len - bits.len();
	println!("read bits len: {read_len}");
	(version_sum, read_len)
}

fn main() {
	let contents = fs::read_to_string("input.txt").unwrap();

	let nums = hex::decode(contents).unwrap();

	let bits = nums.view_bits::<Msb0>();

	let (version_sum, _read_len) = read_packet(bits);

	println!("RESULT: {}", version_sum);
}
