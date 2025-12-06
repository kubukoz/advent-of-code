package main

import (
	"aoc2025/shared"
	"fmt"
	"math"
	"slices"
	"strings"
)

type Battery struct {
	joltage int
}

type Bank struct {
	batteries []Battery
}

func parse(input string) []Bank {
	lines := strings.Split(input, "\n")

	var banks []Bank = make([]Bank, len(lines))
	for i, line := range lines {
		var batteries []Battery = make([]Battery, len(line))
		for j, ch := range line {
			joltage := int(ch - '0')
			batteries[j] = Battery{joltage}
		}
		banks[i] = Bank{batteries}
	}

	return banks
}

func findBest(banks []Bank, joltagesPerBank int) int {
	joltageSum := 0

	for _, bank := range banks {
		best := bestSequence(bank.batteries, joltagesPerBank, func(b Battery) int { return b.joltage })
		slices.Reverse(best)
		joltageSum += sumBatteries(best)
	}

	return joltageSum
}

func bestSequence[T any](choices []T, n int, compareBy func(T) int) []T {
	if n == 0 {
		return []T{}
	}

	var maxValue T = choices[0]
	var maxIndex int = 0

	for i, v := range choices[:len(choices)-n+1] {
		if compareBy(v) > compareBy(maxValue) {
			maxValue = v
			maxIndex = i
		}
	}

	rest := bestSequence(choices[maxIndex+1:], n-1, compareBy)

	rest = append(rest, maxValue)

	return rest
}

func sumBatteries(bats []Battery) int {
	sum := 0
	count := len(bats)

	for i, bat := range bats {
		sum += bat.joltage * int(math.Pow(10, float64(count-i-1)))
	}

	return sum
}

func part1(banks []Bank) int {
	return findBest(banks, 2)
}

func part2(banks []Bank) int {
	return findBest(banks, 12)
}

func main() {
	var input string
	input = "987654321111111\n811111111111119\n234234234234278\n818181911112111"
	input = shared.ReadFile("input.txt")

	banks := parse(input)

	fmt.Printf("Part 1: %v\n", part1(banks))
	fmt.Printf("Part 2: %v\n", part2(banks))
}
