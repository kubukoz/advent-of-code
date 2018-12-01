package com.kubukoz.adventofcode2017

import cats.effect.{ExitCode, IO, IOApp, Sync}
import cats.implicits._
import fs2._

object Day1 extends IOApp {

  case class State[S](previous: Set[S], hasRepeated: Boolean)

  def findFirstRepeated[F[_]: Sync, S](as: Stream[F, S]): F[Option[S]] = {
    as.zipWithScan(Set.empty[S])(_ + _)
      .collectFirst { case (a, seen) if seen(a) => a }
      .compile
      .last
  }

  override def run(args: List[String]): IO[ExitCode] = {
    import cats.effect.Console.io._

    val input = fileLines[IO]("/day1.txt").takeWhile(_.nonEmpty).map(_.toInt)

    val task1 = input.foldMonoid.compile.last
    val task2 = findFirstRepeated(input.repeat.scan(0)(_ + _))

    (task1, task2)
      .parMapN(putStrLn(_) *> putStrLn(_))
      .flatten
      .as(ExitCode.Success)
  }
}
