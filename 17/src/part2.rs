fn calc_x_velocities(target: (i64, i64)) -> Vec<i64> {
	let mut res = Vec::new();
	for i in 0..10_000 {
		let mut pos = 0;
		let mut velocity = i;
		loop {
			pos += velocity;
			if velocity > 0 {
				velocity -= 1;
			}

			if pos >= target.0 && pos <= target.1 {
				res.push(i);
				break;
			}
			if pos > target.1 || velocity == 0 {
				break;
			}
		}
	}
	res
}

fn calc_y_velocities(target: (i64, i64)) -> Vec<i64> {
	let mut res = Vec::new();
	for i in -10_000..10_000 {
		let mut pos = 0;
		let mut velocity = i;
		let mut highest_y = 0;
		loop {
			pos += velocity;
			velocity -= 1;

			if pos > highest_y {
				highest_y = pos;
			}

			if pos >= target.0 && pos <= target.1 {
				res.push(i);
				break;
			}
			if pos < target.0 {
				break;
			}
		}
	}
	res
}

const fn simulate(mut velocity: (i64, i64), target_x: (i64, i64), target_y: (i64, i64)) -> bool {
	let mut pos = (0, 0);
	loop {
		pos.0 += velocity.0;
		pos.1 += velocity.1;

		if velocity.0 > 0 {
			velocity.0 -= 1;
		}

		velocity.1 -= 1;

		if pos.0 >= target_x.0 && pos.0 <= target_x.1 && pos.1 >= target_y.0 && pos.1 <= target_y.1
		{
			return true;
		}
		if pos.0 > target_x.1 || pos.1 < target_y.0 {
			break;
		}
	}
	false
}

fn main() {
	let x_target = (150, 193);
	let y_target = (-136, -86);

	let xs = calc_x_velocities(x_target);
	let ys = calc_y_velocities(y_target);

	let mut velocities = Vec::new();
	for x in &xs {
		for y in &ys {
			if simulate((*x, *y), x_target, y_target) {
				velocities.push((*x, *y));
			}
		}
	}

	println!("xs: {xs:?}");
	println!("ys: {ys:?}");
	println!("vel: {:?}", velocities);
	println!("count: {:?}", velocities.len());
}
