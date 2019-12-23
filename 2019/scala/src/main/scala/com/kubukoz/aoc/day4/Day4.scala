package com.kubukoz.aoc.day4

import cats.effect.{ExitCode, IO, IOApp}
import cats.effect.Console.io._
import cats.implicits._
import cats.kernel.Semigroup
import io.chrisdavenport.monoids.All

object Day4 extends IOApp {

  val input = "264360-746325"

  val digitCount: String => Int = _.length

  val adjacentDigitGroupLengths: String => List[Int] = s => {
    fs2.Stream.emits(s).groupAdjacentBy(identity).map(_._2.size).toList
  }

  def hasAdjacentSameDigits(matchCount: Int => Boolean): String => Boolean =
    adjacentDigitGroupLengths.map(_.exists(matchCount))
  val hasDecreasingDigits: String => Boolean = _.toSeq.sliding(2).exists(s => s.last < s.head)

  implicit val boolSemigroup: Semigroup[Boolean] = _ && _

  val part1: Int => Boolean = {
    digitCount.map(_ === 6) |+|
      hasAdjacentSameDigits(_ >= 2) |+|
      hasDecreasingDigits.map(!_)
  }.compose(_.toString)

  val part2: Int => Boolean = {
    digitCount.map(_ === 6) |+|
      hasAdjacentSameDigits(_ == 2) |+|
      hasDecreasingDigits.map(!_)
  }.compose(_.toString)

  def run(args: List[String]): IO[ExitCode] = {
    val input = 264360 to 746325
    putStrLn(input count part1) *>
      putStrLn(input count part2) as ExitCode.Success
  }

  //    putStrLn(part1(input)) *>
//      putStrLn(part2(input))
}
