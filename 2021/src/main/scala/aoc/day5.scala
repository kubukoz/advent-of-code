package aoc

import cats.implicits._

import lib._

object Day5 extends App {
  case class Point(x: Int, y: Int)

  // not the best name but whatever
  def rangeSymmetric(x: Int, y: Int): Range = {
    val List(min, max) = List(x, y).sorted
    min to max
  }

  case class Line(from: Point, to: Point) {

    def allPoints: List[Point] =
      if (sameX) {
        rangeSymmetric(from.y, to.y).map(y => from.copy(y = y)).toList
      } else if (sameY) {
        rangeSymmetric(from.x, to.x).map(x => from.copy(x = x)).toList
      } else {
        val List(start, end) = List(from, to).sortBy(_.x)

        val ySign =
          if (start.y < end.y)
            1
          else
            -1

        (
          (start.x to end.x).toList,
          (start.y to end.y by ySign).toList,
        ).parMapN(Point.apply)
      }

    def sameX = from.x == to.x
    def sameY = from.y == to.y
  }

  val parse: List[String] => List[Line] = _.map { case s"$ax,$ay -> $bx,$by" =>
    Line(Point(ax.toInt, ay.toInt), Point(bx.toInt, by.toInt))
  }

  val result: List[Line] => Int = _.zipWithIndex
    .flatMap { case (line, index) => line.allPoints.tupleRight(index) }
    .groupBy(_._1)
    .map(_.map(_.size))
    .count(_._2 >= 2)

  val noDiagonals: List[Line] => List[Line] = _.filter(line => line.sameX || line.sameY)

  val part1 = result.compose(noDiagonals).compose(parse)
  val part2 = result.compose(parse)

  val exampleData = readAllLines("day5-example.txt")

  assertEquals(part1(exampleData), 5, "Part 1 example")
  assertEquals(part2(exampleData), 12, "Part 2 example")

  val data = readAllLines("day5.txt")

  assertEquals(part1(data), 5084, "Part 1")
  assertEquals(part2(data), 17882, "Part 2")
}
