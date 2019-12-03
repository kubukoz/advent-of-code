package com.kubukoz.adventofcode2018

import cats.Defer
import cats.effect._
import cats.implicits._
import cats.temp.par._
import fs2.Stream

import scala.annotation.tailrec

object Day5 extends IOApp {
  val CapsDiff: Int = 'a' - 'A' //32

  def uncons(s: String): Option[(Char, String)] = s match {
    case "" => None
    case _  => Some((s.head, s.tail))
  }

  @tailrec
  def singleRound(str: String,
                  prefix: String,
                  flag: Boolean): (String, Boolean) = {
    uncons(str) match {
      case Some((head, tail)) =>
        uncons(tail) match {
          case Some((second, rest)) =>
            if ((head - second).abs == CapsDiff) {
              singleRound(rest, prefix, flag = true)
            } else {
              singleRound(tail, s"$prefix$head", flag)
            }
          case None => (prefix + str, flag)
        }
      case None => (prefix, flag)
    }
  }

  @tailrec
  def part1(input: String): String = {
    singleRound(input, "", flag = false) match {
      case (result, true) =>
        part1(result)
      case (result, false) => result
    }
  }

  def part2[F[_]: Concurrent: ConsoleOut](input: String): F[Option[Int]] = {
    val allPolymers = 'A' to 'Z'

    Stream
      .emits(allPolymers)
      .covary[F]
      .map(char => input.replaceAll(s"[$char${char.toLower}]", ""))
      .mapAsyncUnordered(maxConcurrent = 4) { trimmed =>
        Defer[F].defer(part1(trimmed).length.pure[F])
      }
      .reduce(_ min _)
      .compile
      .last
  }

  override def run(args: List[String]): IO[ExitCode] = {
    implicit val console = cats.effect.Console.io
    import console._

    fileLines[IO]("/day5.txt")
      .filter(_.nonEmpty)
      .evalTap[IO] { input =>
        (IO(part1(input).length), part2[IO](input))
          .parMapN(
            (p1, p2) => putStrLn(s"part 1: $p1") *> putStrLn(s"part 2: $p2")
          )
          .flatten
      }
      .compile
      .drain
      .as(ExitCode.Success)
  }
}
