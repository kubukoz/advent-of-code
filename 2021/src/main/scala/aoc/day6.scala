package aoc

import aoc.lib._
import cats.implicits._
import scala.annotation.tailrec

object Day6 extends App {

  def solve(input: List[String]): (Long, Long) = {
    val parsed = input.mkString.split(",").map(_.toInt).toList

    def goAll(rounds: Int): Long = {
      def recurse(generation: Map[Int, Long], rounds: Int): Long =
        if (rounds == 0)
          generation.values.sum
        else {
          val newGeneration = generation.toList.foldMap {
            case (0, amount)    => Map(6 -> amount, 8 -> amount)
            case (fish, amount) => Map(fish - 1 -> amount)
          }

          recurse(newGeneration, rounds - 1)
        }

      recurse(parsed.groupBy(identity).map(_.map(_.size.toLong)), rounds)
    }

    (80, 256).bimap(goAll, goAll)
  }

  def solveAlt(input: List[String]): (Long, Long) = {
    val parsed = input.mkString.split(",").map(_.toInt).toList

    def goAll(rounds: Int): Long = {

      @tailrec
      def recurse(generation: Map[Int, Long], rounds: Int): Long =
        if (rounds == 0)
          generation.values.sum
        else {
          // foldMap - combines using the Map monoid
          val newGeneration = generation.toList.foldMap {
            case (0, amount)    => Map(6 -> amount, 8 -> amount)
            case (fish, amount) => Map(fish - 1 -> amount)
          }

          recurse(newGeneration, rounds - 1)
        }

      // Credit to @kumalg for the idea of grouping
      // https://github.com/kumalg/advent-of-code-2021/blob/main/typescript/src/days/day06.ts
      recurse(parsed.groupBy(identity).map(_.map(_.size.toLong)), rounds)
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
