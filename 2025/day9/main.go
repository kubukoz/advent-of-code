package main

import (
	"aoc2025/shared"
	"fmt"
	"strconv"
	"strings"
)

func main() {
	input := shared.ReadFile("sample.txt")
	input = shared.ReadFile("input.txt")

	data := parse(input)

	fmt.Printf("Part 1: %v\n", part1(data))
}

type Point2 = shared.Point2

func part1(data []Point2) int {
	maxSoFar := 0
	for _, p1 := range data {
		for _, p2 := range data {
			if p1 == p2 {
				continue
			}

			area := (p1.XDist(p2) + 1) * (p1.YDist(p2) + 1)
			if area > maxSoFar {
				maxSoFar = area
			}
		}
	}

	return maxSoFar
}

func parse(input string) (points []Point2) {
	for _, line := range strings.Split(input, "\n") {
		line := strings.Split(line, ",")
		x, err := strconv.Atoi(line[0])
		if err != nil {
			panic(err)
		}

		y, err := strconv.Atoi(line[1])
		if err != nil {
			panic(err)
		}

		points = append(points, Point2{X: x, Y: y})
	}

	return
}
