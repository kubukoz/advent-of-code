package main

import (
	"fmt"
	"math"
	"os"
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

func part1(banks []Bank) int {
	joltageSum := 0

	for _, bank := range banks {
		maxJoltage := 0

		for firstIndex, firstBattery := range bank.batteries {
			for _, secondBattery := range bank.batteries[firstIndex+1:] {
				maxJoltage = max(
					maxJoltage,
					firstBattery.joltage*10+secondBattery.joltage,
				)

			}
		}

		joltageSum += maxJoltage
	}

	return joltageSum

}

func part2(banks []Bank) int {
	joltageSum := 0

	for _, bank := range banks {
		maxJoltage := 0
		sequences := subsequences(bank.batteries, []Battery{}, 12, func(b Battery) int { return b.joltage })

		for _, sequence := range sequences {
			maxJoltage = max(
				maxJoltage,
				sumBatteries(sequence),
			)
		}
		joltageSum += maxJoltage
	}

	return joltageSum
}

func sumBatteries(bats []Battery) int {
	sum := 0
	count := len(bats)

	for i, bat := range bats {
		sum += bat.joltage * int(math.Pow(10, float64(count-i-1)))
	}

	return sum
}

// this doesn't actually work at all lmao
// it's way too slow. But it's correct!
func subsequences[T any](choices []T, prefix []T, n int, cmp func(T) int) [][]T {

	if n == 0 {
		return [][]T{prefix}
	}
	if n > len(choices) {
		panic("not allowed")
	}

	var results [][]T

	for i, current := range choices[:len(choices)-n+1] {

		var candidates [][]T

		newPrefix := make([]T, len(prefix)+1)
		copy(newPrefix, prefix)
		newPrefix[len(prefix)] = current

		candidates = subsequences(
			choices[i+1:],
			newPrefix,
			n-1,
			cmp,
		)

		results = slices.Concat(results, candidates)
	}

	return results
}

func intSubsequences(choices []int, n int) [][]int {
	return subsequences(choices, []int{}, n, func(i int) int { return i })
}

func main() {

	var input string
	input = "987654321111111\n811111111111119\n234234234234278\n818181911112111"
	// input = readFile("input.txt")

	banks := parse(input)

	fmt.Printf("Part 1: %v\n", part1(banks))
	fmt.Printf("Part 2: %v\n", part2(banks))
}

func readFile(name string) string {
	data, err := os.ReadFile(name)

	// say the line bart
	if err != nil {
		panic(err)
	}

	return strings.TrimSpace(string(data))
}
