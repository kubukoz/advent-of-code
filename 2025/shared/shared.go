package shared

import (
	"math"
	"os"
	"strings"
)

func ReadFile(name string) string {
	data, err := os.ReadFile(name)

	// say the line bart
	if err != nil {
		panic(err)
	}

	return strings.TrimSpace(string(data))
}

type Point3 struct {
	X, Y, Z int
}

func (p Point3) Distance(another Point3) float64 {
	return math.Pow(
		float64(
			absSquare(p.X, another.X)+
				absSquare(p.Y, another.Y)+
				absSquare(p.Z, another.Z),
		),
		1./3,
	)
}

func absSquare(a int, b int) int {
	abs := int(math.Abs(float64(a - b)))
	return abs * abs
}

type Point2 struct{ X, Y int }

func (p Point2) YDist(p2 Point2) int {
	d := p.X - p2.X

	if d < 0 {
		return -d
	} else {
		return d
	}
}

func (p Point2) XDist(p2 Point2) int {
	d := p.Y - p2.Y

	if d < 0 {
		return -d
	} else {
		return d
	}
}
