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

	fmt.Printf("Part 1: %v\n", part1(lines))
}

func part1(lines []string) (total int) {
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

		for pointer := range currentByteIndices {
			atPointer := line[pointer]
			switch atPointer {
			case '.':
				addToCounts(&newCharIndices, pointer)
			case '^':
				addToCounts(&newCharIndices, pointer-1)
				addToCounts(&newCharIndices, pointer+1)

				splitCount++
			}
		}
		currentByteIndices = newCharIndices
	}

	return splitCount
}

func addToCounts[T comparable](items *map[T]int, k T) {
	currV := (*items)[k]
	currV++
	(*items)[k] = currV
}
