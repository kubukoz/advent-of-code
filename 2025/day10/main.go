package main

import (
	"aoc2025/shared"
	"fmt"
	"strconv"
	"strings"
)

func main() {
	input := shared.ReadFile("sample.txt")
	// input = shared.ReadFile("input.txt")

	data := parse(input)

	fmt.Printf("I SPENT ALL DAY PARSING AND ALL I GOT WAS SOME CRAP: %v\n", data)
}

func parse(input string) (machines []Machine) {
	for line := range strings.SplitSeq(input, "\n") {
		fields := strings.Fields(line)

		var targetState MachineState
		for _, rune := range fields[0][1 : len(fields[0])-1] {
			switch rune {
			case '#':
				targetState.states = append(targetState.states, true)
			case '.':
				targetState.states = append(targetState.states, false)
			default:
				panic(fmt.Sprintf("unexpected rune %c", rune))
			}
		}

		var buttons []Button
		for _, buttonField := range fields[1 : len(fields)-1] {
			numbers := []int{}
			for number := range strings.SplitSeq(buttonField[1:len(buttonField)-1], ",") {
				number, err := strconv.Atoi(number)
				if err != nil {
					panic(err)
				}
				numbers = append(numbers, number)
			}

			buttons = append(buttons, Button{numbers})
		}

		machines = append(machines, Machine{targetState, buttons})
	}
	return
}

type Machine struct {
	targetState MachineState
	buttons     []Button
	// keeping this for part 2
	// joltages    []Joltage
}

type MachineState struct {
	states []bool
}

type Button struct {
	affectsIndices []int
}

type Joltage = int

// [.###.#] (0,1,2,3,4) (0,3,4) (0,1,2,4,5) (1,2) {10,11,11,5,10,5}
