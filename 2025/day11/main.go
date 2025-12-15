package main

import (
	"aoc2025/shared"
	"fmt"
	"strings"
)

func main() {
	input := shared.ReadFile("sample.txt")
	input = shared.ReadFile("input.txt")

	data := parse(input)

	fmt.Printf("Part 1: %v\n", part1(data))
}

func part1(descriptors Descriptors) int {
	currentPoints := make(map[string]int)
	currentPoints["you"] = 1

	next := make(map[string]bool)
	for currentPoint := range currentPoints {
		next[currentPoint] = true
	}

	for {
		// fmt.Printf("state: %v\n", currentPoints)
		if len(next) == 0 {
			return currentPoints["out"]
		}

		newNext := make(map[string]bool)
		for k := range next {
			neighbors := descriptors[k]
			for _, neighbor := range neighbors {
				// fmt.Printf("Adding %v from %v to %v (%v)\n", currentPoints[k], k, neighbor, currentPoints[neighbor])
				currentPoints[neighbor] += currentPoints[k]
			}

			for _, n := range neighbors {
				newNext[n] = true
			}
		}

		next = newNext
	}
}

func parse(input string) Descriptors {
	descriptors := make(Descriptors)

	for line := range strings.SplitSeq(input, "\n") {

		splitted := strings.Split(line, ":")

		descriptors[splitted[0]] = strings.Fields(splitted[1])
	}
	return descriptors
}

type Descriptors map[string][]string
