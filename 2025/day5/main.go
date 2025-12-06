package main

import (
	"aoc2025/shared"
	"fmt"
	"slices"
	"strconv"
	"strings"
)

func main() {
	var input string
	input = "3-5\n10-14\n16-20\n12-18\n\n1\n5\n8\n11\n17\n32"

	input = shared.ReadFile("input.txt")

	parts := strings.Split(input, "\n\n")

	ranges := parseRanges(strings.Split(parts[0], "\n"))
	ingredients := parseIngredients(strings.Split(parts[1], "\n"))

	fmt.Printf("Part 1: %v\n", part1(ranges, ingredients))
	fmt.Printf("Part 2: %v\n", part2(ranges))
}

func part1(ranges []Range, ingredients []int) int {
	count := 0

	for _, ingredient := range ingredients {
		fresh := false
		for _, r := range ranges {
			if r.fits(ingredient) {
				fresh = true
				break
			}
		}

		if fresh {
			count++
		}
	}

	return count
}

func part2(ranges []Range) (count int) {
	visitedRanges := mergeOverlapping(ranges)

	for _, r := range visitedRanges {
		count += r.size()
	}

	return count
}

func mergeOverlapping(ranges []Range) (visitedRanges []Range) {
	for _, nextRange := range ranges {
		overlappings, overlappingIndices := findOverlappings(visitedRanges, nextRange)

		var combined Range
		if len(overlappings) > 0 {
			combined = joinRanges(overlappings)
			combined = combined.join(nextRange)

			visitedRanges = removeIndices(overlappingIndices, visitedRanges)
		} else {
			combined = nextRange
		}

		visitedRanges = append(visitedRanges, combined)
	}

	return visitedRanges
}

func removeIndices(indicesOrdered []int, items []Range) []Range {
	slices.Reverse(indicesOrdered)

	for _, i := range indicesOrdered {
		items = slices.Delete(items, i, i+1)
	}
	return items
}

func findOverlappings(visitedRanges []Range, nextRange Range) (overlappings []Range, overlappingIndices []int) {
	for i, visitedRange := range visitedRanges {
		if nextRange.overlaps(visitedRange) {
			overlappings = append(overlappings, visitedRange)
			overlappingIndices = append(overlappingIndices, i)
		}
	}
	return overlappings, overlappingIndices
}

func joinRanges(ranges []Range) (combined Range) {
	combined = ranges[0]

	for _, r := range ranges[1:] {
		combined = combined.join(r)
	}
	return combined
}

func parseRanges(lines []string) (ranges []Range) {

	for _, line := range lines {
		r := strings.Split(line, "-")

		from, err := strconv.Atoi(r[0])
		if err != nil {
			panic(err)
		}
		to, err := strconv.Atoi(r[1])
		if err != nil {
			panic(err)
		}

		ranges = append(ranges, Range{fromInc: from, toInc: to})
	}

	return ranges
}

func parseIngredients(lines []string) (ingredients []int) {
	for _, line := range lines {
		i, err := strconv.Atoi(line)
		if err != nil {
			panic(err)
		}
		ingredients = append(ingredients, i)
	}

	return ingredients
}

type Range struct {
	fromInc int
	toInc   int
}

func (r Range) fits(number int) bool {
	return number >= r.fromInc && number <= r.toInc
}

func (r Range) size() int {
	return r.toInc - r.fromInc + 1
}

func (r Range) overlaps(another Range) bool {
	return r.fromInc <= another.toInc &&
		r.toInc >= another.fromInc
}

func (r Range) join(another Range) Range {
	return Range{
		fromInc: min(r.fromInc, another.fromInc),
		toInc:   max(r.toInc, another.toInc),
	}
}
