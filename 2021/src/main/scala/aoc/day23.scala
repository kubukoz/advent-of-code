package aoc

import cats.data.NonEmptyList
import cats.implicits._

import scala.annotation.tailrec
import scala.collection.immutable
import lib._

object Day23 extends App {

  sealed trait Node extends Product with Serializable
  case class Corridor(index: Int) extends Node

  case class Room(index: Int) extends Node {
    val letter: Char = ('A' to 'D')(index)
  }

  val corridor = (1 to 7).indices.map(Corridor.apply).toList

  val rooms =
    (1 to 4)
      .indices
      .map { i =>
        Room(i)
      }
      .toList

  case class Edge(from: Node, to: Node, dist: Int) {
    def flip = Edge(to, from, dist)
  }

  val edges = List(
    corridor(1) -> 1 -> corridor(0),
    corridor(1) -> 2 -> rooms(0),
    corridor(1) -> 2 -> corridor(2),
    corridor(2) -> 2 -> rooms(0),
    corridor(2) -> 2 -> rooms(1),
    corridor(2) -> 2 -> corridor(3),
    corridor(3) -> 2 -> rooms(1),
    corridor(3) -> 2 -> rooms(2),
    corridor(3) -> 2 -> corridor(4),
    corridor(4) -> 2 -> rooms(2),
    corridor(4) -> 2 -> rooms(3),
    corridor(4) -> 2 -> corridor(5),
    corridor(5) -> 2 -> rooms(3),
    corridor(5) -> 1 -> corridor(6),
  ).map { case ((from, dist), to) => Edge(from, to, dist) }

  val edgesFrom: Map[Node, List[Edge]] =
    edges.groupBy(_.from) |+| edges.groupBy(_.to).map(_.map(_.map(_.flip)))

  case class Pod(name: Char) {
    def multiplier = Map('A' -> 1, 'B' -> 10, 'C' -> 100, 'D' -> 1000)(name)
  }

//we parse to this
  val initState = State(
    Map(
      rooms(0) -> NonEmptyList.of(Pod('B'), Pod('A')),
      rooms(1) -> NonEmptyList.of(Pod('C'), Pod('D')),
      rooms(2) -> NonEmptyList.of(Pod('B'), Pod('C')),
      rooms(3) -> NonEmptyList.of(Pod('D'), Pod('A')),
    ),
    0L,
  )

  case class NewState(pending: Map[Node, Pod], totalCost: Long)

  sealed trait Moves extends Product with Serializable

  case class Finished(distance: Int) extends Moves
  case class ToCorridors(possibleTargets: Map[Corridor, Int]) extends Moves
  import util.chaining._

  def movesForPod(pod: Pod, loc: Node, usedNodes: Set[Node]): Moves = {

    val neighs = edgesFrom(loc)

    neighs
      .collectFirst {
        case Edge(_, target @ Room(_), dist) if target.letter == pod.name => Finished(dist)
      }
      .getOrElse {
        neighs
          .collect { case Edge(_, target @ Corridor(_), dist) => (target, dist) }
          .filter { case (target, _) => !usedNodes(target) }
          .toMap
          .pipe(ToCorridors(_))
      }
  }

  case class State(pods: Map[Node, NonEmptyList[Pod]], cost: Long) {

    def at(pos: Node): Char = ??? // pods.get(pos).fold('.')(_.name)

    def isFinished: Boolean =
      pods == Map(
        rooms(0) -> NonEmptyList.of(Pod('A'), Pod('A')),
        rooms(1) -> NonEmptyList.of(Pod('B'), Pod('B')),
        rooms(2) -> NonEmptyList.of(Pod('C'), Pod('C')),
        rooms(3) -> NonEmptyList.of(Pod('D'), Pod('D')),
      )

  }

  var minCostSoFar = Long.MaxValue

// moveOnce(initState)

  // println(recurse(initState, Nil))
  println(minCostSoFar)

  assertEquals(
    movesForPod(Pod('B'), rooms(0), Set.empty),
    ToCorridors(Map(corridor(1) -> 2, corridor(2) -> 2)),
    "Yes moves for pod in the wrong room",
  )

  assertEquals(
    movesForPod(Pod('B'), corridor(3), Set.empty),
    Finished(2),
    "Yes moves for pod in corridor next to its room",
  )

  assertEquals(
    movesForPod(Pod('B'), corridor(5), Set.empty),
    ToCorridors(Map(corridor(4) -> 2, corridor(6) -> 1)),
    "Yes moves for pod in corridor next to no useful room",
  )

  assertEquals(
    movesForPod(Pod('B'), corridor(6), Set.empty),
    ToCorridors(Map(corridor(5) -> 1)),
    "Yes moves for pod in last corridor",
  )

  assertEquals(
    movesForPod(Pod('B'), corridor(6), Set(corridor(5))),
    ToCorridors(Map.empty),
    "No moves if corridor next to pod is taken",
  )

// val input = readAllLines("day23-example.txt")

// val pods = input
//   .drop(2)
//   .init
//   .zipWithIndex
//   .map {
//     case (line, 0) => line.drop(2).dropRight(2)
//     case (line, _) => line
//   }
//   .map(_.split("#").filterNot(_.trim.isEmpty()).toList)
}
