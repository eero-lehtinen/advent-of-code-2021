use std::fmt;
use std::fs;

#[derive(Clone, Debug)]
enum Pixel {
	Light,
	Dark,
}

struct InfiniteImage {
	data: Vec<Vec<Pixel>>, // Pixels inside data area
	outside: Pixel,        // Lightness of pixels outside of data area
}

impl InfiniteImage {
	fn new(data: Vec<Vec<Pixel>>, outside: Pixel) -> Self {
		let mut data = data;
		// Add padding
		for line in data.iter_mut() {
			for _ in 0..3 {
				line.insert(0, outside.clone());
				line.push(outside.clone());
			}
		}
		for _ in 0..3 {
			data.insert(0, vec![outside.clone(); data[0].len()]);
			data.push(vec![outside.clone(); data[0].len()]);
		}

		InfiniteImage { data, outside }
	}

	fn get(&self, x: usize, y: usize) -> &Pixel {
		self.data
			.get(y)
			.and_then(|d| d.get(x))
			.expect("valid index")
	}

	fn get_enchancement_index(&self, x: usize, y: usize) -> usize {
		let mut res: usize = 0;
		for i in y - 1..=y + 1 {
			for j in x - 1..=x + 1 {
				res <<= 1;
				match self.get(j, i) {
					Pixel::Light => res += 1,
					Pixel::Dark => {}
				}
			}
		}
		res
	}

	fn iterate(&self, enhancement: &[Pixel]) -> Self {
		let new_data = (1..self.data.len() - 1)
			.map(|y| {
				(1..self.data[0].len() - 1)
					.map(|x| {
						let idx = self.get_enchancement_index(x, y);
						enhancement[idx].clone()
					})
					.collect::<Vec<_>>()
			})
			.collect::<Vec<_>>();

		let new_outside = match self.outside {
			Pixel::Light => enhancement.last(),
			Pixel::Dark => enhancement.first(),
		}
		.unwrap()
		.clone();

		InfiniteImage::new(new_data, new_outside)
	}
}

impl fmt::Display for InfiniteImage {
	fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
		let x = self
			.data
			.iter()
			.map(|line| {
				line.iter()
					.map(|p| match p {
						Pixel::Light => '#',
						Pixel::Dark => '.',
					})
					.collect::<String>()
			})
			.collect::<Vec<_>>()
			.join("\n");
		write!(f, "{x}")
	}
}

const ITERS: i32 = 50;

fn main() {
	let contents = fs::read_to_string("input.txt").unwrap();
	let mut lines = contents.lines();

	let char_to_pixel = |c| match c {
		'#' => Pixel::Light,
		'.' => Pixel::Dark,
		_ => panic!("Unexpected character"),
	};

	let enhancement = lines
		.next()
		.unwrap()
		.chars()
		.map(char_to_pixel)
		.collect::<Vec<_>>();

	let mut image = InfiniteImage::new(
		lines
			.skip(1)
			.map(|line| line.chars().map(char_to_pixel).collect())
			.collect(),
		Pixel::Dark,
	);

	for _ in 0..ITERS {
		// println!("{image}");
		image = image.iterate(&enhancement);
	}

	println!("{image}");

	let sum: u64 = image.data.iter().fold(0, |sum, line| {
		sum + line.iter().fold(0, |sum, p| {
			sum + match p {
				Pixel::Light => 1,
				Pixel::Dark => 0,
			}
		})
	});

	println!("Lit pixels: {sum}");
}
