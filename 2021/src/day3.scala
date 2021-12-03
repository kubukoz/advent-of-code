import cats.implicits._

import scala.util.chaining._
import scala.annotation.tailrec

object Day3 extends App {

  sealed trait Mode

  case object Max extends Mode
  case object Min extends Mode

  type Data = List[List[Boolean]]

  val charToBool: Char => Boolean = {
    case '0' => false
    case '1' => true
  }

  def mkBinString(bools: List[Boolean]) =
    bools.map {
      if (_)
        1
      else
        0
    }.mkString

  def matchingBits(data: Data, mode: Mode): String = data
    .transpose
    .map(_.groupBy(identity).map(_.map(_.size)))
    .map { withCount =>
      mode match {
        case Max => withCount.maxBy(_._2)
        case Min => withCount.minBy(_._2)
      }
    }
    .map(_._1)
    .pipe(mkBinString)

  def parseBin(s: String) = Integer.parseInt(s, 2)

  def part1(data: Data): Int = {
    val mostCommons = matchingBits(data, Max)
    val leastCommons = matchingBits(data, Min)

    parseBin(mostCommons) * parseBin(leastCommons)
  }

  def part2(data: Data): Int = {

    @tailrec
    def go(bitIndex: Int, data: Data, mode: Mode): List[Boolean] = {

      val matchingCurrentBit = matchingBits(data, mode)

      // matching/nonmatching based on the top current bit
      // if this is equal, we'll tie break later
      val (matchingBit, nonMatchingBit) = data
        .partition(_.apply(bitIndex) == matchingCurrentBit(bitIndex))

      val expectedBitAfterTieBreak: Boolean =
        mode match {
          case _ if matchingBit.size != nonMatchingBit.size =>
            matchingCurrentBit(bitIndex).pipe(charToBool)
          case Max => true
          case Min => false
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

    parseBin(r1.pipe(mkBinString)) * parseBin(r2.pipe(mkBinString))
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

  val data: Data = inputTest
    .map(_.map(charToBool))

  println("Part 1: " + part1(data))
  println("Part 2: " + part2(data))
}
