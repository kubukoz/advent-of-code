package aoc

import aoc.lib._
import cats.data.State
import cats.implicits._
import cats.mtl.Stateful
import cats.Monad

object Day6 extends App {

  type CacheMap = Map[(Int, Int), Long]

  def go[F[_]: Monad](
    fish: Int,
    rounds: Int,
  )(
    implicit S: Stateful[F, CacheMap]
  ): F[Long] = {
    def store(result: Long) = S.modify(_ + ((fish, rounds) -> result))

    S.get.flatMap {
      _.get((fish, rounds))
        .toOptionT[F]
        .getOrElseF {
          if (rounds == 0)
            1L.pure[F] /* no more rounds, count this one fish */
          else {
            if (fish == 0) {
              (
                go[F](6, rounds - 1),
                go[F](8, rounds - 1),
              ).mapN(_ + _)
            } else
              go[F](fish - 1, rounds - 1)
          }.flatTap(store)
        }
    }
  }

  def solve(input: List[String]): (Long, Long) = {
    val parsed = input.mkString.split(",").map(_.toInt).toList

    def goAll(rounds: Int): Long =
      parsed
        .foldMapM(go[State[CacheMap, *]](_, rounds))
        .runA(Map.empty)
        .value

    (80, 256).bimap(goAll, goAll)
  }

  val (exampleRound1, exampleRound2) = solve(readAllLines("day6-example.txt"))

  assertEquals(exampleRound1, 5934L, "Example round 1")
  assertEquals(exampleRound2, 26984457539L, "Example round 2")

  val (round1, round2) = solve(readAllLines("day6.txt"))
  assertEquals(round1, 362740L, "Round 1")
  assertEquals(round2, 1644874076764L, "Round 2")
}
