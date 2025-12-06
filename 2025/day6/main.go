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

	fmt.Printf("Part 1: %v\n", part1(parse1(input)))
}

func part1(table Table) int {
	sum := 0

	for _, column := range table.columns {

		value := column.initValue()

		for _, number := range column.inputs {
			number, err := strconv.Atoi(strings.TrimSpace(number))
			if err != nil {
				panic(err)
			}

			value = column.op(value, number)
		}

		sum += value
	}

	return sum
}

func parse1(text string) Table {
	lines := strings.Split(text, "\n")

	lastLine := lines[len(lines)-1]
	operatorIndices := findOperators(lastLine)
	rowLines := lines[:(len(lines) - 1)]

	columns := make([]Column, len(operatorIndices))

	for _, line := range rowLines {
		for columnIndex, startIndex := range operatorIndices {
			isLast := columnIndex == len(operatorIndices)-1

			var endIndex int
			if isLast {
				endIndex = len(line)
			} else {
				endIndex = operatorIndices[columnIndex+1] - 1
			}

			word := line[startIndex:endIndex]

			columns[columnIndex].inputs = append(columns[columnIndex].inputs, word)
		}
	}

	for columnIndex, opIndex := range operatorIndices {
		columns[columnIndex].opChar = lastLine[opIndex]
	}

	return Table{columns}
}

func findOperators(text string) (indices []int) {
	for i, ch := range text {
		if ch != ' ' {
			indices = append(indices, i)
		}
	}

	return
}

type Table struct {
	columns []Column
}

type Column struct {
	inputs []string
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
