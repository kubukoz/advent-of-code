package main

import (
	"fmt"
	"os"
	"strings"
)

func main() {
	var input string
	input = "..@@.@@@@.\n@@@.@.@.@@\n@@@@@.@.@@\n@.@@@@..@.\n@@.@@@@.@@\n.@@@@@@@.@\n.@.@.@.@@@\n@.@@@.@@@@\n.@@@@@@@@.\n@.@.@@@.@."

	input = readFile("input.txt")

	lines := strings.Split(input, "\n")

	count := 0
	for lineIndex, line := range lines {
		for charIndex, char := range line {
			neighborsFlashing := 0

			if char != '@' {
				continue
			}

			for dy := -1; dy <= 1; dy++ {
				for dx := -1; dx <= 1; dx++ {
					y := lineIndex + dy
					x := charIndex + dx

					if !(dx == 0 && dy == 0) && inRange(y, 0, len(lines)-1) && inRange(x, 0, len(line)-1) {
						neighbor := lines[y][x]
						if neighbor == '@' {
							neighborsFlashing++
						}
					}
				}
			}

			if neighborsFlashing < 4 {
				count++
			}
		}
	}

	fmt.Println(count)
}

func inRange(number int, fromInc int, toInc int) bool {
	return number >= fromInc && number <= toInc
}

func readFile(name string) string {
	data, err := os.ReadFile(name)

	// say the line bart
	if err != nil {
		panic(err)
	}

	return strings.TrimSpace(string(data))
}
