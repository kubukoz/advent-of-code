package aoc

import aoc.lib._
import cats.Monad
import cats.data.OptionT
import cats.data.State
import cats.implicits._
import cats.mtl.Stateful

import util.chaining._

object Day6 extends App {

  final case class CacheMap(cache: Map[(Int, Int), Long], hits: Long, misses: Long) {
    def +(kv: (((Int, Int), Long))): CacheMap = copy(cache = cache + kv)

    def get(k: (Int, Int)): Option[Long] = cache.get(k)
  }

  def go[F[_]: Monad](
    fish: Int,
    rounds: Int,
  )(
    implicit S: Stateful[F, CacheMap]
  ): F[Long] = {
    def store(result: Long) = S.modify(_ + ((fish, rounds) -> result))

    def updateStats(result: Option[_]): F[Unit] =
      result
        .fold(
          S.modify(s => s.copy(misses = s.misses + 1))
        )(_ => S.modify(s => s.copy(hits = s.hits + 1)))

    S.get.flatMap {
      _.get((fish, rounds))
        .pure[F]
        .flatTap(updateStats(_))
        .pipe(OptionT(_))
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

    def goAll(rounds: Int): Long = {
      val (s, result) =
        parsed
          .foldMapM(go[State[CacheMap, *]](_, rounds))
          .run(CacheMap(Map.empty, 0, 0))
          .value

      println(
        s"Cache hits: ${s.hits}, misses: ${s.misses}, hits/total rate: ${(s.hits * 100) / (s.hits + s.misses)}%"
      )

      result
    }
    (80, 256).bimap(goAll, goAll)
  }

  val (exampleRound1, exampleRound2) = solve(readAllLines("day6-example.txt"))

  assertEquals(exampleRound1, 5934L, "Example round 1")
  assertEquals(exampleRound2, 26984457539L, "Example round 2")

  val (round1, round2) = solve(readAllLines("day6.txt"))
  assertEquals(round1, 362740L, "Round 1")
  assertEquals(round2, 1644874076764L, "Round 2")
}
