package aoc

import aoc.lib._
import cats.implicits._
import util.chaining._

object Day13 extends App {

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

  def parse(input: String): (Set[(Int, Int)], List[Fold]) = {
    val Array(positionString, foldString) = input.split("\n\n")

    val positions =
      positionString
        .split("\n")
        .map { case s"$x,$y" => (x.toInt, y.toInt) }
        .toSet

    val folds =
      foldString
        .split("\n")
        .map {
          case s"fold along x=$x" => Horizontal(x.toInt)
          case s"fold along y=$y" => Vertical(y.toInt)
        }
        .toList

    (positions, folds)
  }

  sealed trait Fold extends Product with Serializable
  case class Horizontal(x: Int) extends Fold
  case class Vertical(y: Int) extends Fold

  def perform(positions: Set[(Int, Int)], fold: Fold) =
    fold match {
      case Horizontal(x) =>
        positions.map { case (px, py) =>
          val newX =
            if (px < x)
              px
            else {
              val diff = px - x

              x - diff
            }

          (newX, py)
        }
      case Vertical(y) =>
        positions.map { case (x, py) =>
          val newY =
            if (py < y)
              py
            else {
              val diff = py - y

              y - diff
            }

          (x, newY)
        }
    }

  def show(positions: Set[(Int, Int)]) = {
    val maxX = positions.map(_._1).max
    val maxY = positions.map(_._2).max

    (0 to maxY)
      .map { y =>
        (0 to maxX).map { x =>
          if (positions.contains((x, y)))
            "#"
          else
            " "
        }.mkString
      }
      .mkString("\n")

  }

  locally {
    val (positions, folds) = readAll("day13-example.txt").pipe(parse)

    val part2Expected =
      """#####
        |#   #
        |#   #
        |#   #
        |#####""".stripMargin

    assertEquals(perform(positions, folds.head).size, 17, "Part 1 (example)")
    assertEquals(
      show(folds.foldLeft(positions)(perform)),
      part2Expected,
      "Part 2 (example)",
      showResult = false,
    )
  }

  locally {
    val (positions, folds) = readAll("day13.txt").pipe(parse)

    val space = " "

    val part2Expected =
      s"""#### ###  #     ##  ###  #  # #    ###$space
         |#    #  # #    #  # #  # #  # #    #  #
         |###  #  # #    #    #  # #  # #    #  #
         |#    ###  #    # ## ###  #  # #    ###$space
         |#    #    #    #  # # #  #  # #    # #$space
         |#### #    ####  ### #  #  ##  #### #  #""".stripMargin

    assertEquals(perform(positions, folds.head).size, 710, "Part 1")
    assertEquals(
      show(folds.foldLeft(positions)(perform)),
      part2Expected,
      "Part 2",
      showResult = false,
    )
  }
}
