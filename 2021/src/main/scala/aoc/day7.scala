package aoc

import lib._

object Day7 extends App {

  val fromFile = readAll("day7.txt")
  val example = readAll("day7-example.txt")

  def solve(input: String, cost: Int => Int) = {
    val data = input.split(",").map(_.toInt).toList

    (data.min to data.max).map { target =>
      data.map(pos => cost((pos - target).abs)).sum
    }.min
  }

  val part1: String => Int = solve(_, identity)
  val part2: String => Int = solve(_, 1.to(_).sum)

  assertEquals(part1(example), 37, "Part 1 example")
  assertEquals(part2(example), 168, "Part 2 example")

  assertEquals(part1(fromFile), 356179, "Part 1 real")
  assertEquals(part2(fromFile), 99788435, "Part 2 real")

}
