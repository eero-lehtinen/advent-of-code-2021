import fs from "fs"

const data = fs.readFileSync("./input01.txt")
const numbers = data
	.toString()
	.split("\n")
	.map(s => parseInt(s, 10))

let count = 0
let lastSum = Infinity
for (let i = 0; i < numbers.length - 2; i++) {
	let sum = numbers[i] + numbers[i + 1] + numbers[i + 2]
	if (sum > lastSum) {
		count++
	}
	lastSum = sum
}
console.log(count)
