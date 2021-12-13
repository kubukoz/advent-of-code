package aoc

import cats.data.NonEmptyList

import aoc.lib._
import cats.implicits._
import scala.util.chaining._

object Day12 extends App {

  case class Edge(from: String, to: String)

  val isEnd: String => Boolean = _ == "end"
  val isSmall: String => Boolean = _.matches("[a-z]+")

  def countPaths(edges: Set[Edge], allowSmallDuplicate: Boolean): Int = {

    val getEdgesFrom: String => Set[String] = {
      val edgesByFrom = edges.groupBy(_.from).map(_.map(_.map(_.to)))
      val edgesByTo = edges.groupBy(_.to).map(_.map(_.map(_.from)))

      (edgesByFrom |+| edgesByTo).withDefaultValue(Set.empty)
    }
    def dedupe(history: NonEmptyList[String]): Set[String] => Set[String] = {
      val smallHistory = history.filter(isSmall)
      def hasSmallDuplicate = smallHistory.groupBy(identity).exists(_._2.size > 1)

      if (!allowSmallDuplicate || hasSmallDuplicate)
        _ -- smallHistory.filter(isSmall)
      else
        identity
    }

    def go(history: NonEmptyList[String]): NonEmptyList[NonEmptyList[String]] =
      if (history.head == "end")
        NonEmptyList.one(history)
      else {
        val possibleNext = getEdgesFrom
          .andThen(_ - "start")
          .andThen(dedupe(history))
          .apply(history.head)

        NonEmptyList(
          history,
          possibleNext.toList.flatMap { next =>
            go(next :: history).toList
          },
        )
      }

    go(NonEmptyList.of("start"))
      .filter(path => isEnd(path.head))
      .size
  }

  def part1(edges: Set[Edge]) = countPaths(edges, false)
  def part2(edges: Set[Edge]) = countPaths(edges, true)

  val parse: List[String] => Set[Edge] =
    _.map(_.split("-") match {
      case Array(a, b) => Edge(a, b)
    }).toSet

  val example = readAllLines("day12-example.txt").pipe(parse)

  assertEquals(part1(example), 10, "Part 1 (example)")
  assertEquals(part2(example), 36, "Part 2 (example)")

  val fromFile = readAllLines("day12.txt").pipe(parse)

  assertEquals(part1(fromFile), 3298, "Part 1")
  assertEquals(part2(fromFile), 93572, "Part 2")

}
