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

	fmt.Printf("Part 1: %v\n", part1(parse(input)))
	fmt.Printf("Part 2: %v\n", part1(transposeColumns(parse(input))))
}

func part1(table Table) (sum int) {
	for _, column := range table.columns {

		value := column.initValue()

		for _, input := range column.inputs {
			number, err := strconv.Atoi(strings.TrimSpace(input))
			if err != nil {
				panic(err)
			}

			value = column.op(value, number)
		}

		sum += value
	}

	return
}

func transposeColumns(table Table) (modified Table) {
	for _, column := range table.columns {
		modified.columns = append(modified.columns, column.transpose())
	}
	return
}

func parse(text string) Table {
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

func (column Column) width() (width int) {
	for _, entry := range column.inputs {
		width = max(width, len(entry))
	}
	return
}
func (column Column) transpose() (newColumn Column) {
	newColumn.opChar = column.opChar

	for indexInColumn := range column.width() {
		var chars []byte

		for _, entry := range column.inputs {
			var char byte
			if indexInColumn >= len(entry) {
				char = ' '
			} else {
				char = entry[indexInColumn]
			}

			chars = append(chars, char)
		}
		input := string(chars)

		newColumn.inputs = append(newColumn.inputs, input)
	}
	return
}
