fn calc_lowest_x_velocity(target_start: i64, target_end: i64) -> i64 {
	for i in 0..target_end {
		let mut pos = 0;
		let mut velocity = i;
		loop {
			pos += velocity;
			if velocity > 0 {
				velocity -= 1;
			}

			if pos >= target_start && pos <= target_end {
				return i;
			}
			if pos > target_end || velocity == 0 {
				break;
			}
		}
	}
	panic!("unreachable");
}

fn calc_highest_y_velocity(target_start: i64, target_end: i64) -> (i64, i64) {
	for i in (0..10_000).rev() {
		let mut pos = 0;
		let mut velocity = i;
		let mut highest_y = 0;
		loop {
			pos += velocity;
			velocity -= 1;

			if pos > highest_y {
				highest_y = pos;
			}

			if pos >= target_start && pos <= target_end {
				return (i, highest_y);
			}
			if pos < target_start {
				break;
			}
		}
	}
	panic!("unreachable");
}

fn main() {
	let x_target = (150, 193);
	let y_target = (-136, -86);

	let x_vel = calc_lowest_x_velocity(x_target.0, x_target.1);
	let (y_vel, highest_y) = calc_highest_y_velocity(y_target.0, y_target.1);

	println!("x_vel: {x_vel:?}");
	println!("y_vel: {y_vel:?}");
	println!("highest_y: {highest_y:?}");
}
