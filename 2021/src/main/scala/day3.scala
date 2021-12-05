import cats.Id

import cats.data.State
import cats.implicits._
import cats.kernel.Order

import scala.util.chaining._

object Day3 extends App {

  sealed trait Mode

  case object Max extends Mode
  case object Min extends Mode

  type Data = List[List[Boolean]]

  object util {

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

    def parseBin(s: List[Boolean]) = Integer.parseInt(s.pipe(mkBinString), 2)
  }

  import util._

  def topBits[A: Order](data: List[List[A]], mode: Mode): List[A] = data
    .transpose
    .map(_.groupByNel(identity).map(_.map(_.size)))
    .map { withCount =>
      mode match {
        case Max => withCount.maxBy(_._2)
        case Min => withCount.minBy(_._2)
      }
    }
    .map(_._1)

  def part1(data: Data): Int = {
    val mostCommons = topBits(data, Max)
    val leastCommons = topBits(data, Min)

    parseBin(mostCommons) * parseBin(leastCommons)
  }

  def part2(data: Data): Int = {
    def keepMatchingWithTieBreak(
      mode: Mode
    )(
      matchBy: Seq[Boolean] => Boolean
    ): State[Data, Unit] = State.modify { data =>
      // The top bits in the remaining data
      val currentTopBits = topBits(data, mode)

      val expectedBitExact = currentTopBits.pipe(matchBy)

      // Says whether the entry has the given bit at the position defined by matchBy
      def entryMatches(bit: Boolean): List[Boolean] => Boolean = _.pipe(matchBy) == bit

      // matching/nonmatching based on the top current bit
      // if this is equal, we'll tie break later
      val (matchingBit, nonMatchingBit) = data.partition(entryMatches(expectedBitExact))

      val expectedBitAfterTieBreak: Boolean =
        mode match {
          case _ if matchingBit.size != nonMatchingBit.size => expectedBitExact
          case Max                                          => true
          case Min                                          => false
        }

      // taking the tie break into account
      data.filter(entryMatches(expectedBitAfterTieBreak))
    }

    def iterate(
      data: Data,
      mode: Mode,
    ): List[Boolean] = (0, data).tailRecM[Id, List[Boolean]] { case (bitIndex, data) =>
      keepMatchingWithTieBreak(mode)(_.apply(bitIndex)).runS(data).value match {
        case result :: Nil => Right(result)
        case filtered      => Left((bitIndex + 1, filtered))
      }
    }
    /*
    def oneRound(bitIndex: Int, mode: Mode): State[Data, Either[List[Boolean], Unit]] =
      keepMatchingWithTieBreak(mode)(_.apply(bitIndex)) *> State.get[Data].map {
        case result :: Nil => Left(result)
        case _             => Right(())
      }

    def iterate2(
      data: Data,
      mode: Mode,
    ): List[Boolean] =
      // Alternative implementation
      data
        .head
        .indices
        .toList
        .traverse_(oneRound(_, mode).pipe(EitherT(_)))
        .swap
        .valueOr(_ => throw new Exception("Didn't exit early!"))
        .runA(data)
        .value
     */

    val r1 = iterate(data, Max)
    val r2 = iterate(data, Min)

    parseBin(r1) * parseBin(r2)
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
