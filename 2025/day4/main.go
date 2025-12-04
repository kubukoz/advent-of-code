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

	fmt.Println(countRolls(lines, false))
	fmt.Println(countRolls(lines, true))
}

func countRolls(lines []string, canRemove bool) int {

	count := 0
	keepRunning := true

	for keepRunning {
		keepRunning = false

		for lineIndex := range lines {
			for charIndex := range lines[lineIndex] {
				neighborsFlashing := 0

				if lines[lineIndex][charIndex] != '@' {
					continue
				}

				for dy := -1; dy <= 1; dy++ {
					for dx := -1; dx <= 1; dx++ {
						y := lineIndex + dy
						x := charIndex + dx

						if !(dx == 0 && dy == 0) &&
							inRange(y, 0, len(lines)) &&
							inRange(x, 0, len(lines[lineIndex])) {

							if lines[y][x] == '@' {
								neighborsFlashing++
							}
						}
					}
				}

				if neighborsFlashing < 4 {
					count++
					if canRemove {
						r := []rune(lines[lineIndex])
						r[charIndex] = '.'
						lines[lineIndex] = string(r)
						keepRunning = true
					}
				}
			}

		}
	}

	return count
}

func inRange(number int, fromInc int, toExc int) bool {
	return number >= fromInc && number < toExc
}

func readFile(name string) string {
	data, err := os.ReadFile(name)

	// say the line bart
	if err != nil {
		panic(err)
	}

	return strings.TrimSpace(string(data))
}
