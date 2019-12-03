package com.kubukoz.adventofcode2018

import cats.effect.{ExitCode, IO, IOApp, Sync}
import cats.implicits._
import mouse.boolean._

object Day2 extends IOApp {
  def part1[F[_]: Sync](inputs: fs2.Stream[F, String]): F[Int] = {
    inputs
      .map(values)
      .compile
      .foldMonoid
      .map { case (twos, threes) => twos * threes }
  }

  def part2[F[_]: Sync](inputs: fs2.Stream[F, String]): F[Option[String]] = {
    inputs.compile.to[Stream].map {
      _.combinations(2).toStream.collectFirstSome {
        case Stream(a, b) =>
          commonLetters(a, b).some.filter(_.length === a.length - 1)
      }
    }
  }

  private def values(input: String): (Int, Int) = {
    val counts: Map[Int, Char] =
      input.toList.groupBy(identity).map(_.map(_.size).swap)

    val hasTwos = counts.isDefinedAt(2) ?? 1
    val hasThrees = counts.isDefinedAt(3) ?? 1

    (hasTwos, hasThrees)
  }

  //assumes strings are of the same size
  private def commonLetters(a: String, b: String): String =
    (a zip b).collect { case (left, right) if left === right => left }.mkString

  import cats.effect.Console.io._

  override def run(args: List[String]): IO[ExitCode] = {
    val inputs = fileLines[IO]("/day2.txt")

    val p1 = part1(inputs)
    val p2 = part2(inputs)

    (p1, p2).mapN(putStrLn(_) *> putStrLn(_)).flatten
  }.as(ExitCode.Success)
}
