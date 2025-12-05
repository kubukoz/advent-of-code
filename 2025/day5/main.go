package main

import (
	"fmt"
	"os"
	"strconv"
	"strings"
)

func main() {
	var input string
	input = "3-5\n10-14\n16-20\n12-18\n\n1\n5\n8\n11\n17\n32"

	input = readFile("input.txt")

	parts := strings.Split(input, "\n\n")

	ranges := parseRanges(strings.Split(parts[0], "\n"))
	ingredients := parseIngredients(strings.Split(parts[1], "\n"))

	fmt.Printf("Part 1: %v\n", part1(ranges, ingredients))
}

func part1(ranges []Range, ingredients []int) int {
	count := 0

	for _, ingredient := range ingredients {
		fresh := false
		for _, r := range ranges {
			if inRange(ingredient, r.fromInc, r.toInc) {
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

func parseRanges(lines []string) []Range {
	var ranges []Range

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

func parseIngredients(lines []string) []int {
	var ingredients []int

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
