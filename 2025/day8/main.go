package main

import (
	"aoc2025/shared"
	"fmt"
	"maps"
	"slices"
	"strconv"
	"strings"
)

type Point3 = shared.Point3

func main() {
	input := shared.ReadFile("sample.txt")
	input = shared.ReadFile("input.txt")

	data := parse(input)

	fmt.Printf("Part 1: %v\n", part1(data, 1000))
}

type Circuit map[Point3]bool

type Pair struct{ p1, p2 Point3 }

type PairWithDistance struct {
	p1, p2   Point3
	distance float64
}

func part1(points []Point3, iterations int) int {
	circuits := makeCircuits(points, iterations)

	fmt.Printf("circuit count: %v\n", len(circuits))

	slices.SortFunc(circuits, func(c1, c2 Circuit) int {
		return len(c2) - len(c1)
	})

	sizeProduct := 1
	for _, c := range circuits[0:3] {
		sizeProduct *= len(c)
	}

	return sizeProduct
}

func makeCircuits(points []Point3, iterations int) (circuits []Circuit) {
	distances := makeDistances(points)

	if iterations > len(points) {
		panic(fmt.Sprintf("illegal iteration count: %v > %v", iterations, len(points)))
	}

	for _, p := range points {
		c := make(Circuit)
		c[p] = true
		circuits = append(circuits, c)
	}

	for i := range iterations {
		pair := distances[i]

		leftIndex := findMatchingCircuit(circuits, pair.p1)
		rightIndex := findMatchingCircuit(circuits, pair.p2)

		if leftIndex == rightIndex {
			// panic(fmt.Sprintf("same index: %v,%v\n", leftIndex, rightIndex))
			continue
		}
		switch {
		case leftIndex >= 0 && rightIndex >= 0:
			// found matches in two distinct circuits
			// append to the one on the left, update it, remove circuit on the right

			// fmt.Printf("Found two matches, combining circuits %v, %v\n", circuits[leftIndex], circuits[rightIndex])
			leftCircuit := circuits[leftIndex]
			maps.Copy(leftCircuit, circuits[rightIndex])
			circuits = slices.Delete(circuits, rightIndex, rightIndex+1)

		case leftIndex >= 0:
			// fmt.Printf("Found match for point %v in circuit %v\n", pair.p2, circuits[leftIndex])
			// found match in left circuit
			circuits[leftIndex][pair.p2] = true

		case rightIndex >= 0:
			// fmt.Printf("Found match for point %v in circuit %v\n", pair.p1, circuits[rightIndex])
			// found in right
			circuits[rightIndex][pair.p1] = true

		default:
			panic("should not happen")
		}

		// fmt.Printf("Current circuit count: %v\n", len(circuits))
		// fmt.Printf("Current circuits: %v\n\n", circuits)
	}

	return
}

func findMatchingCircuit(circuits []Circuit, point Point3) int {
	for i, circuit := range circuits {
		if circuit[point] {
			return i
		}
	}

	return -1
}
func makeDistances(points []Point3) (distanceList []PairWithDistance) {
	distanceMap := make(map[Pair]float64)

	for _, p1 := range points {
		for _, p2 := range points {
			_, presentFlipped := distanceMap[Pair{p2, p1}]
			if p1 == p2 || presentFlipped {
				continue
			}
			distanceMap[Pair{p1, p2}] = p1.Distance(p2)
		}
	}

	for pair, dist := range distanceMap {
		distanceList = append(distanceList, PairWithDistance{pair.p1, pair.p2, dist})
	}

	slices.SortFunc(distanceList, func(p1, p2 PairWithDistance) int {
		// TODO: let's hope this is accurate enough xD
		return int((p1.distance - p2.distance) * 10000)
	})
	return
}

func parse(input string) (results []Point3) {
	for _, line := range strings.Split(input, "\n") {
		coords := []int{}
		for _, coord := range strings.Split(line, ",") {
			parsed, err := strconv.Atoi(coord)
			if err != nil {
				panic(err)
			}
			coords = append(coords, parsed)
		}

		results = append(results, Point3{X: coords[0], Y: coords[1], Z: coords[2]})

	}
	return
}
