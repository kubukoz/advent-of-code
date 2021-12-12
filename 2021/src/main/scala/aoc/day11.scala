package aoc

import scala.annotation.tailrec

import cats.implicits._
import aoc.lib._

object Day11 extends App {
  type Data = Map[(Int, Int), Int]
  type Flashed = Set[(Int, Int)]

  def step(data: Data): (Data, Flashed) = {
    def siblingPositions(x: Int, y: Int): Set[(Int, Int)] =
      ((-1 to 1).toList, (-1 to 1).toList).mapN((dx, dy) => (x + dx, y + dy)).toSet - ((x, y))

    @tailrec
    def go(data: Data, alreadyFlashed: Flashed): (Data, Flashed) = {
      val flashingNow = (data -- alreadyFlashed).filter { case (_, v) => v > 9 }.keySet

      if (flashingNow.isEmpty)
        (data, alreadyFlashed)
      else {
        val siblingUpdates = flashingNow.toList.foldMap { case (x, y) =>
          siblingPositions(x, y)
            .filter(data.contains)
            .map(pos => (pos, 1))
            .toMap
        }

        go(data |+| siblingUpdates, alreadyFlashed ++ flashingNow)
      }
    }

    val (newData, newFlashed) = go(data.view.mapValues(_ + 1).toMap, Set.empty)

    (newData ++ newFlashed.map(pos => (pos, 0)), newFlashed)
  }

  def solve(data: Data) =
    LazyList.iterate((data, Set.empty: Flashed)) { case (d, c) => step(d) }.tail

  def part1(data: Data): Int = solve(data).take(100).foldMap(_._2.size)

  def part2(data: Data) = solve(data).zipWithIndex.collectFirst {
    case ((d, flashed), i) if flashed.size == data.size => i + 1
  }

  val parse: List[String] => Data =
    _.map(_.toList)
      .zipWithIndex
      .flatMap { case (line, y) =>
        line.zipWithIndex.map { case (ch, x) => ((x, y) -> ch.toString.toInt) }
      }
      .toMap

  val example = parse(readAllLines("day11-example.txt"))
  assertEquals(part1(example), 1656, "Part 1 (example)")
  assertEquals(part2(example), 195.some, "Part 2 (example)")

  val fromFile = parse(readAllLines("day11.txt"))
  assertEquals(part1(fromFile), 1667, "Part 1")
  assertEquals(part2(fromFile), 488.some, "Part 2")
}
