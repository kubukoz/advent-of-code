import cats.implicits._

import aoc.lib._

val inputExample = readAllLines("day5-example.txt")
val inputReal = readAllLines("day5.txt")

// val input = inputExample
val input = inputReal

case class Point(x: Int, y: Int)

case class Line(from: Point, to: Point) {

  def allPoints =
    if (sameX) {
      {
        if (from.y > to.y)
          (to.y to from.y)
        else
          (from.y to to.y)
      }.map(y => from.copy(y = y))
    } else if (sameY) {
      if (from.x > to.x)
        (to.x to from.x)
      else
        (from.x to to.x)
    }.map(x => from.copy(x = x))
    else {
      // here, the line is diagonal
      // inc both x and y until both points are equal
      // start with smaller x and y

      // from.x, from.y -> to.x, to.y
      // 9,7 -> 7,9 covers points 7,9, 9,7, 8,8, and 9,7



      // from.x goes up
      // from.y goes down

      val minX = from.x min to.x
      val maxX = from.x max to.x

      val minY = from.y min to.y
      val maxY = from.y max to.y
      val diffX = (from.x - to.x).abs

      (minX to maxX).zipWithIndex.map { case (x, i) =>

        val goingFromFrom = minX == from.x

        val y = if(goingFromFrom) {
          if(minY == from.y)
            minY + i
          else maxY - i
        } else {
          // going from "to"
          if(minY == to.y)
            minY + i
          else maxY - i
        }

        Point(x, y)
      }
    }

  def sameX = from.x == to.x
  def sameY = from.y == to.y
}

val data = input
  .map { case s"$ax,$ay -> $bx,$by" => Line(Point(ax.toInt, ay.toInt), Point(bx.toInt, by.toInt)) }
  // .filter { line =>
  //   line.sameX ||
  //   line.sameY
  // }
  .zipWithIndex
  .flatMap { case (line, index) =>
    // point -> line id...
    line.allPoints.map(_ -> index)
  }
  .groupBy(_._1)
  .map(_.map(_.size))
  // .values
  .count(_._2 >= 2)

// Line(Point(9,7), Point( 7,9)).allPoints
//
