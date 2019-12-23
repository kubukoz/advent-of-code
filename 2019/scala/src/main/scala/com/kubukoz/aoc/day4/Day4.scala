package com.kubukoz.aoc.day4

import cats.effect.{ExitCode, IO, IOApp}
import cats.effect.Console.io._
import cats.implicits._
import cats.kernel.Semigroup

object Day4 extends IOApp {
  implicit val boolSemigroup: Semigroup[Boolean] = _ && _

  object filters {
    val digitCount: String => Int = _.length

    val adjacentDigitGroupLengths: String => List[Int] =
      fs2.Stream.emits(_).groupAdjacentBy(identity).map(_._2.size).toList

    def hasAdjacentSameDigits(matchCount: Int => Boolean): String => Boolean =
      adjacentDigitGroupLengths.map(_.exists(matchCount))

    val hasDecreasingDigits: String => Boolean = _.toSeq.sliding(2).exists(s => s.last < s.head)
  }

  import filters._

  val part1: String => Boolean =
    digitCount.map(_ === 6) |+|
      hasAdjacentSameDigits(_ >= 2) |+|
      hasDecreasingDigits.map(!_)

  val part2: String => Boolean =
    digitCount.map(_ === 6) |+|
      hasAdjacentSameDigits(_ == 2) |+|
      hasDecreasingDigits.map(!_)

  def run(args: List[String]): IO[ExitCode] = {
    val input = 264360 to 746325 map (_.toString)

    putStrLn(input count part1) *>
      putStrLn(input count part2)
  } as ExitCode.Success
}
