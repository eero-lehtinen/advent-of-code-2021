use std::fs;

struct Node {
	idx: usize,
	name: String,
	neighbors: Vec<usize>,
	big: bool,
}

fn insert_node(nodes: &mut Vec<Node>, node_name: &str) -> usize {
	let node: Node = Node {
		idx: nodes.len(),
		big: node_name.chars().next().unwrap().is_uppercase(),
		neighbors: Vec::new(),
		name: node_name.to_string(),
	};
	nodes.push(node);
	nodes.len() - 1
}

fn print_path(nodes: &[Node], path: &[usize]) {
	println!(
		"{}",
		path.iter()
			.map(|idx| nodes[*idx].name.clone())
			.collect::<Vec<String>>()
			.join(",")
	);
}

fn main() {
	let contents = fs::read_to_string("input.txt").unwrap();

	let edges: Vec<(&str, &str)> = contents
		.lines()
		.map(|line| {
			let mut split = line.split('-');
			(split.next().unwrap(), split.next().unwrap())
		})
		.collect();

	let mut nodes: Vec<Node> = Vec::new();

	for edge in edges {
		let idx1 = nodes
			.iter()
			.position(|node| node.name == edge.0)
			.unwrap_or_else(|| insert_node(&mut nodes, edge.0));

		let idx2 = nodes
			.iter()
			.position(|node| node.name == edge.1)
			.unwrap_or_else(|| insert_node(&mut nodes, edge.1));

		nodes[idx1].neighbors.push(idx2);
		nodes[idx2].neighbors.push(idx1);
	}

	let start_idx = nodes
		.iter()
		.find(|n| n.name == "start")
		.map(|n| n.idx)
		.unwrap();
	let end_idx = nodes
		.iter()
		.find(|n| n.name == "end")
		.map(|n| n.idx)
		.unwrap();

	let mut paths: Vec<Vec<usize>> = Vec::new();

	let mut stack: Vec<(usize, Vec<usize>, bool)> = vec![(start_idx, Vec::new(), false)];

	while !stack.is_empty() {
		let (idx, path, visited_twice) = stack.pop().unwrap();
		let mut new_path = path.clone();
		new_path.push(idx);

		let node = &nodes[idx];
		if idx != end_idx {
			for neigh_idx in &node.neighbors {
				let neigh = &nodes[*neigh_idx];

				if neigh.big {
					stack.push((*neigh_idx, new_path.clone(), visited_twice));
					continue;
				}

				let count = new_path.iter().filter(|i| *i == neigh_idx).count();
				if count == 0 {
					stack.push((*neigh_idx, new_path.clone(), visited_twice));
				} else if count == 1
					&& !visited_twice && *neigh_idx != start_idx
					&& *neigh_idx != end_idx
				{
					stack.push((*neigh_idx, new_path.clone(), true));
				}
			}
		} else {
			paths.push(new_path);
		}
	}

	for path in &paths {
		print_path(&nodes, path);
	}

	println!("{}", paths.len());
}
