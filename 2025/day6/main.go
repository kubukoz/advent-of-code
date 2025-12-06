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

	fmt.Printf("Part 1: %v\n", run(parse1(input)))
}

func run(table Table) int {
	sum := 0

	for _, column := range table.columns {

		value := column.initValue()

		for _, number := range column.inputs {
			value = column.op(value, number)
		}

		sum += value
	}

	return sum
}

func parse1(text string) Table {
	lines := strings.Split(text, "\n")
	lastLine := strings.Fields(lines[len(lines)-1])
	rowLines := lines[:(len(lines) - 1)]

	columns := make([]Column, len(lastLine))

	for _, line := range rowLines {
		numbers := strings.Fields(line)

		for columnIndex, number := range numbers {
			number, err := strconv.Atoi(number)
			if err != nil {
				panic(err)
			}

			columns[columnIndex].inputs = append(columns[columnIndex].inputs, number)
		}
	}

	for columnIndex, op := range lastLine {
		columns[columnIndex].opChar = op[0]
	}

	return Table{columns}
}

type Table struct {
	columns []Column
}

type Column struct {
	inputs []int
	opChar byte
}

func (c Column) op(a, b int) int {
	switch c.opChar {
	case '+':
		return a + b
	case '*':
		return a * b
	default:
		panic("invalid op")
	}
}

func (c Column) initValue() int {
	switch c.opChar {
	case '+':
		return 0
	case '*':
		return 1
	default:
		panic("invalid op")
	}
}
