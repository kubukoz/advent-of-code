package main

import (
	"aoc2025/shared"
	"fmt"
	"slices"
	"strconv"
	"strings"
)

func main() {
	input := shared.ReadFile("sample.txt")
	// input = shared.ReadFile("input.txt")

	data := parse(input)

	fmt.Printf("Part 1: %v\n", part1(data))
}

func part1(machines []Machine) (sum int) {
	for _, m := range machines {
		sum += solve(m)
	}
	return
}

func solve(machine Machine) int {
	states := []MachineState{machine.initState()}
	steps := 0

	if states[0].sameStates(machine.targetState) {
		return 0
	}

	for {
		nextRound := []MachineState{}
		steps++

		for _, nextButton := range machine.buttons {

			for _, previousState := range states {
				newState := MachineState{slices.Clone(previousState.states)}

				// Surely this can be done faster by treating the state as a bitmask and treating buttons as masks
				// but part 1 passes fine and I didn't feel like doing that in Go just yet
				for _, indexToUpdate := range nextButton.affectsIndices {
					newState.states[indexToUpdate] = !newState.states[indexToUpdate]
				}

				if newState.sameStates(machine.targetState) {
					return steps
				}

				nextRound = append(nextRound, newState)
			}
		}

		states = nextRound
	}
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

func (m Machine) initState() (s MachineState) {
	for range m.targetState.states {
		s.states = append(s.states, false)
	}
	return
}

type MachineState struct {
	states []bool
}

func (m MachineState) sameStates(state MachineState) bool {
	if len(m.states) != len(state.states) {
		return false
	}

	for i, m1 := range m.states {
		if state.states[i] != m1 {
			return false
		}
	}
	return true
}

type Button struct {
	affectsIndices []int
}

type Joltage = int

// [.###.#] (0,1,2,3,4) (0,3,4) (0,1,2,4,5) (1,2) {10,11,11,5,10,5}
