package aoc

import cats.implicits._
import lib._
import scala.collection.immutable

object Day9 extends App {

  case class Points(value: Map[(Int, Int), Int]) {
    val heights: immutable.Iterable[Int] = value.map(_._2)

    lazy val lowPoints: Points = Points(
      value.filter { case ((x, y), ch) => ch < siblings(x, y).map(_._2).min }
    )

    lazy val basins: List[Set[((Int, Int), Int)]] = lowPoints.value.toList.map {
      case ((x, y), level) => findBasin(x, y, level)
    }

    private def siblings(x: Int, y: Int) = List(
      (-1, 0),
      (1, 0),
      (0, -1),
      (0, 1),
    )
      .map { case (dx, dy) => (dx, dy) }
      .flatMap { case (dx, dy) => value.get((x + dx, y + dy)).tupleLeft((x + dx, y + dy)) }

    private def findBasin(x: Int, y: Int, level: Int): Set[((Int, Int), Int)] =
      if (level == 9)
        Set.empty
      else {
        siblings(x, y)
          .filter(_._2 > level)
          .foldMap { next =>
            val ((nx, ny), nextLevel) = next

            findBasin(nx, ny, nextLevel)
          } + ((x, y) -> level)
      }

    lazy val visualize: String = {
      val missingPoints = value -- basins.flatMap(_.map(_._1))

      val sizeX = value.map(_._1._1).max
      val sizeY = value.map(_._1._2).max

      val colors = List(
        Console.CYAN,
        Console.MAGENTA,
        Console.YELLOW,
        Console.BLUE,
        Console.RED,
        Console.WHITE,
      )

      (0 until sizeY)
        .map { y =>
          (0 until sizeX).map { x =>
            val height = value((x, y))

            if (lowPoints.value.contains((x, y)))
              s"${Console.GREEN}$height${Console.RESET}"
            else if (height == 9)
              " "
            else {
              val basinIndex = basins.indexWhere(_.map(_._1).contains((x, y)))
              val color = colors(basinIndex % colors.size)

              s"$color$height${Console.RESET}"
            }
          }.mkString
        }
        .mkString("\n")

    }

  }

  def parse(input: List[String]): Points = Points {
    input
      .zipWithIndex
      .flatMap { case (line, y) =>
        line.zipWithIndex.map { case (ch, x) => (x, y) -> ch.toString.toInt }
      }
      .toMap
  }

  val example = readAllLines("day9-example.txt")
  val fromFile = readAllLines("day9.txt")

  val part1: Points => Int = _.lowPoints.heights.toList.foldMap(_ + 1)
  val part2: Points => Int = _.basins.map(_.size).sorted.takeRight(3).product

  assertEquals(part1.compose(parse)(example), 15, "Part 1 example")
  assertEquals(part2.compose(parse)(example), 1134, "Part 2 example")

  assertEquals(part1.compose(parse)(fromFile), 588, "Part 1")
  assertEquals(part2.compose(parse)(fromFile), 964712, "Part 2")

  // Files.writeString(Paths.get("day9-viz.txt"), points.visualize)

}
