import fs from "fs"

const data = fs.readFileSync("./input02.txt")
const lines = data.toString().split("\n")

let pos = 0
let depth = 0
let aim = 0

for (const line of lines) {
	const [command, numStr] = line.split(" ")
	const num = parseInt(numStr, 10)

	if (command === "forward") {
		pos += num
		depth += aim * num
	} else if (command === "down") {
		aim += num
	} else if (command === "up") {
		aim -= num
	}
}

console.log(pos * depth)
