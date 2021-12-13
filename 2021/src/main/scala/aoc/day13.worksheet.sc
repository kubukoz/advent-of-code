import java.nio.file.Paths

import java.nio.file.Files
import cats.implicits._

import aoc.lib._

val example = """6,10
0,14
9,10
0,3
10,4
4,11
6,0
6,12
4,1
0,13
10,12
3,4
3,0
8,4
1,10
2,14
8,10
9,0

fold along y=7
fold along x=5"""

val input = readAll("day13.txt")
// val input = example

val Array(positionString, foldString) = input.split("\n\n")

val positions =
  positionString
    .split("\n")
    .map(_.split(",") match {
      case Array(x, y) => (x.toInt, y.toInt)
    })
    .map(pos => (pos, 1))
    .toMap

sealed trait Fold extends Product with Serializable
case class Horizontal(x: Int) extends Fold
case class Vertical(y: Int) extends Fold

val folds =
  foldString
    .split("\n")
    .map {
      case s"fold along x=$x" => Horizontal(x.toInt)
      case s"fold along y=$y" => Vertical(y.toInt)
    }
    .toList

def perform(positions: Map[(Int, Int), Int], fold: Fold) =
  fold match {
    case Horizontal(x) =>
      positions.toList.foldMap { case ((px, py), value) =>
        val newX =
          if (px < x)
            px
          else {
            val diff = px - x

            x - diff
          }

        Map(
          (newX, py) -> value
        )

      }
    case Vertical(y) =>
      positions.toList.foldMap { case ((x, py), value) =>
        val newY =
          if (py < y)
            py
          else {
            val diff = py - y

            y - diff
          }

        Map(
          (x, newY) -> value
        )

      }
  }

def show(positions: Map[(Int, Int), Int]) = {
  val maxX = positions.map(_._1._1).max
  val maxY = positions.map(_._1._2).max

  (0 to maxY)
    .map { y =>
      (0 to maxX).map { x =>
        positions.get((x, y)) match {
          case Some(_) => "#"
          case _       => " "
        }
      }.mkString
    }
    .mkString("\n")

}

println(show(folds.foldLeft(positions)(perform)))
