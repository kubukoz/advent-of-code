package main

import (
	"aoc2025/shared"
	"fmt"
	"slices"
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

	currentByteIndices := []int{startIndex}
	splitCount := 0

	for {
		currentLineIndex++
		if currentLineIndex == len(lines) {
			break
		}
		line := lines[currentLineIndex]

		newCharIndices := []int{}

		for _, pointer := range currentByteIndices {
			atPointer := line[pointer]
			switch atPointer {
			case '.':
				newCharIndices = appendIfMissing(newCharIndices, pointer)
			case '^':
				// if pointer > 0 {
				newCharIndices = appendIfMissing(newCharIndices, pointer-1)
				// }
				// if pointer < len(line) {
				newCharIndices = appendIfMissing(newCharIndices, pointer+1)
				// }
				splitCount++
			}
		}
		currentByteIndices = newCharIndices
	}

	return splitCount
}

// not very efficient but what can you do
func appendIfMissing[T comparable](slice []T, item T) []T {
	if slices.Contains(slice, item) {
		return slice
	} else {
		return append(slice, item)
	}
}
