import cats.implicits._

import scala.annotation.tailrec

object Day3 extends App {

  sealed trait Mode

  case object Max extends Mode
  case object Min extends Mode

  def matchingBits(data: List[List[Char]], mode: Mode): String =
    data
      .transpose
      .map(_.groupBy(identity).map(_.map(_.size)))
      .map { withCount =>
        mode match {
          case Max => withCount.maxBy(_._2)
          case Min => withCount.minBy(_._2)
        }
      }
      .map(_._1)
      .mkString

  def parseBin(s: String) = Integer.parseInt(s, 2)

  def part1(data: List[List[Char]]): Int = {
    val mostCommons = matchingBits(data, Max)
    val leastCommons = matchingBits(data, Min)

    parseBin(mostCommons) * parseBin(leastCommons)
  }

  def part2(data: List[List[Char]]): Int = {

    @tailrec
    def go(bitIndex: Int, data: List[List[Char]], mode: Mode): List[Char] = {

      val matchingCurrentBit = matchingBits(data, mode)

      // matching/nonmatching based on the top current bit
      // if this is equal, we'll tie break later
      val (matchingBit, nonMatchingBit) = data
        .partition(_.apply(bitIndex) == matchingCurrentBit(bitIndex))

      val expectedBitAfterTieBreak: Char =
        mode match {
          case _ if matchingBit.size != nonMatchingBit.size => matchingCurrentBit(bitIndex)
          case Max                                          => '1'
          case Min                                          => '0'
        }

      // taking the tie break into account
      val filtered = data.filter(_.apply(bitIndex) == expectedBitAfterTieBreak)

      if (filtered.size == 1)
        filtered.head
      else {
        go(bitIndex + 1, filtered, mode)
      }
    }

    val r1 = go(0, data, Max)
    val r2 = go(0, data, Min)

    parseBin(r1.mkString) * parseBin(r2.mkString)
  }

  val inputTest = """00100
11110
10110
10111
10101
01111
00111
11100
10000
11001
00010
01010""".split("\n").toList.map(_.toList)

  val inputReal = lib.readAllLines("day3.txt").map(_.toList)

  val data = inputTest

  println("Part 1: " + part1(data))
  println("Part 2: " + part2(data))
}
