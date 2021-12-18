package aoc

import cats.implicits._

import scala.annotation.tailrec
import scala.collection.mutable.PriorityQueue

import util.chaining._
import aoc.lib._
import java.{util => ju}

object Day15 extends App {

  type Data[A] = Map[(Int, Int), A]
  def lookup[A](data: Data[A], x: Int, y: Int) = data(x, y)
  def lookupSafe[A](data: Data[A], x: Int, y: Int) = data.get(x, y)

  def mapify[A](as: List[List[A]]): Map[(Int, Int), A] =
    as.zipWithIndex
      .flatMap { case (line, y) => line.zipWithIndex.map { case (a, x) => (x, y) -> a } }
      .toMap

  def findTarget[A](data: Map[(Int, Int), A]): (Int, Int) = {
    val targetX = data.keys.map(_._1).max
    val targetY = data.keys.map(_._2).max
    (targetX, targetY)
  }

  val convertInts: List[List[Char]] => List[List[Int]] = _.map(_.map(_.toString.toInt))

  def shortestPath(
    data: Data[Int],
    from: (Int, Int),
    to: (Int, Int),
  ): Long = {
    val withNeighbors: Data[Set[(Int, Int)]] = {
      val diffs = Set((-1, 0), (1, 0), (0, -1), (0, 1))

      data.map { case (coords @ (x, y), _) =>
        coords -> diffs
          .map { case (dx, dy) => (x + dx, y + dy) }
          .flatMap { case c @ (x, y) => lookupSafe(data, x, y).as(c) }
      }
    }

    val totalSize = data.size
    val oneperc = totalSize / 100

    @tailrec
    def go(unvisited: Set[(Int, Int)], distances: Data[Double]): Long =
      if (unvisited.isEmpty)
        distances(to).toLong
      else {
        val current = unvisited.minBy(distances)

        val selfDistance = distances(current)
        val unvisitedNeighbors = withNeighbors(current).filter(unvisited)

        val newDistances =
          unvisitedNeighbors.map { coord =>
            (coord, distances(coord) min (selfDistance + data(coord)))
          }.toMap

        go(
          unvisited = unvisited - current,
          distances = distances ++ newDistances,
        )
      }

    go(
      unvisited = data.keySet,
      distances = data.map { case (coords, _) => coords -> Double.PositiveInfinity } + (from -> 0),
    )
  }

  def solve(input: List[List[Char]]) = {
    val parsed = input.pipe(convertInts)

    val source = (0, 0)
    val data = parsed.pipe(mapify)
    val part1 = shortestPath(data, source, findTarget(data))

    val data2 = multiply(parsed, 5).pipe(mapify)
    val part2 = shortestPath(data2, source, findTarget(data2))

    (part1, part2)
  }

  def multiply(input: List[List[Int]], n: Int): List[List[Int]] = {

    def inc(n: Int): Int =
      if (n == 9)
        1
      else
        n + 1
    val firstLine = List
      .iterate(input, n)(_.nested.map(inc).value)
      .reduce((_, _).parMapN(_ ++ _))

    List.iterate(firstLine, n)(_.nested.map(inc).value).flatten
  }

  locally {
    val example = readAllLines("day15-example.txt").map(_.toList)
    val (part1, part2) = solve(example)
    assertEquals(solve(example), (40, 315), "Example")
  }

  if (false)
    locally {
      val fromFile = aoc.lib.readAllLines("day15.txt").map(_.toList)
      val (part1, part2) = solve(fromFile)

      println(part2)
      // assertEquals(solve(fromFile),(707,))
    }
}
