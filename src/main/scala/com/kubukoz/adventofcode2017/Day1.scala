package com.kubukoz.adventofcode2017

import cats.effect.{ExitCode, IO, IOApp, Sync}
import cats.implicits._
import fs2._

object Day1 extends IOApp {

  case class State[S](latest: S, previous: Set[S], hasRepeated: Boolean)

  def find2[F[_]: Sync, A, S](
    as: Stream[F, A]
  )(zero: S)(f: (S, A) => S): F[Option[S]] = {
    as.repeat
      .scan(State(zero, Set.empty[S], hasRepeated = false)) {
        case (State(s, mem, _), a) =>
          val newS = f(s, a)
          State(newS, mem + newS, mem.contains(newS))
      }
      .find(_.hasRepeated)
      .map(_.latest)
      .compile
      .last
  }

  override def run(args: List[String]): IO[ExitCode] = {
    import cats.effect.Console.io._

    val input = fileLines[IO]("/day1.txt").takeWhile(_.nonEmpty).map(_.toInt)

    val task1 = input.foldMonoid.compile.last
    val task2 = find2(input)(0)(_ + _)

    (task1, task2)
      .parMapN(putStrLn(_) *> putStrLn(_))
      .flatten
      .as(ExitCode.Success)
  }
}
