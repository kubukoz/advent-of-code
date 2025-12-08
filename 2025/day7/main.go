package main

import (
	"aoc2025/shared"
	"fmt"
	"strings"
)

func main() {
	input := shared.ReadFile("sample.txt")
	input = shared.ReadFile("input.txt")
	lines := strings.Split(input, "\n")

	part1, part2 := both(lines)
	fmt.Printf("Part 1: %v, Part 2: %v\n", part1, part2)
}

func both(lines []string) (part1 int, part2 int) {
	// assume the start is in the first line
	startIndex := strings.IndexByte(lines[0], 'S')
	currentLineIndex := 0

	currentByteIndices := map[int]int{startIndex: 1}
	splitCount := 0

	for {
		currentLineIndex++
		if currentLineIndex == len(lines) {
			break
		}
		line := lines[currentLineIndex]

		newCharIndices := map[int]int{}

		for pointer, countAtPointer := range currentByteIndices {
			atPointer := line[pointer]
			switch atPointer {
			case '.':
				newCharIndices[pointer] += countAtPointer
			case '^':
				newCharIndices[pointer-1] += countAtPointer
				newCharIndices[pointer+1] += countAtPointer

				splitCount++
			}
		}

		currentByteIndices = newCharIndices
	}

	totalPaths := 0
	for _, c := range currentByteIndices {
		totalPaths += c
	}

	return splitCount, totalPaths
}
