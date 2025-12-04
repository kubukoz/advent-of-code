package main

import (
	"fmt"
	"os"
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
	// find largest joltage in each bank, sum them up
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

func main() {
	var input string
	// input = "987654321111111\n811111111111119\n234234234234278\n818181911112111"

	data, err := os.ReadFile("input.txt")

	// say the line bart
	if err != nil {
		panic(err)
	}

	input = strings.TrimSpace(string(data))

	banks := parse(input)

	fmt.Printf("Part 1: %v", part1(banks))
}
