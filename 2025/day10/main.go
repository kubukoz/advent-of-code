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
				newState := MachineState{previousState.state ^ nextButton.mask}

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

		targetState := MachineState{0}
		for i, rune := range fields[0][1 : len(fields[0])-1] {
			switch rune {
			case '#':
				targetState.state += (1 << i)
			case '.':
				// do nothing
			default:
				panic(fmt.Sprintf("unexpected rune %c", rune))
			}
		}

		var buttons []Button
		for _, buttonField := range fields[1 : len(fields)-1] {
			numbers := uint(0)
			for number := range strings.SplitSeq(buttonField[1:len(buttonField)-1], ",") {
				number, err := strconv.Atoi(number)
				if err != nil {
					panic(err)
				}
				numbers += (1 << number)
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
	return MachineState{0}
}

type MachineState struct {
	state uint
}

func (m MachineState) sameStates(state MachineState) bool {
	return m.state == state.state
}

type Button struct {
	mask uint
}

type Joltage = int

// [.###.#] (0,1,2,3,4) (0,3,4) (0,1,2,4,5) (1,2) {10,11,11,5,10,5}
