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
  )

  def neighbors(node: Node): List[(Int, Node)] = edges.collect {
    case ((from, dist), to) if from == node => (dist, to)
    case ((from, dist), to) if to == node   => (dist, from)
  }

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

  case class State(pods: Map[Node, NonEmptyList[Pod]], cost: Long) {

    def at(pos: Node): Char = ??? // pods.get(pos).fold('.')(_.name)

    def isFinished: Boolean =
      pods == Map(
        rooms(0) -> NonEmptyList.of(Pod('A'), Pod('A')),
        rooms(1) -> NonEmptyList.of(Pod('B'), Pod('B')),
        rooms(2) -> NonEmptyList.of(Pod('C'), Pod('C')),
        rooms(3) -> NonEmptyList.of(Pod('D'), Pod('D')),
      )

    def render: String = List(
      "#" * 13,
      "#"
        + at(corridor(0))
        + at(corridor(1))
        + '.'
        + at(corridor(2))
        + '.'
        + at(corridor(3))
        + '.'
        + at(corridor(4))
        + '.'
        + at(corridor(5))
        + at(corridor(6))
        + '#',
      "###"
        + at(rooms(0))
        + '#'
        + at(rooms(2))
        + '#'
        + at(rooms(4))
        + '#'
        + at(rooms(6))
        + "###",
      "  #"
        + at(rooms(1))
        + '#'
        + at(rooms(3))
        + '#'
        + at(rooms(5))
        + '#'
        + at(rooms(7))
        + "#  ",
      "  #########  ",
      "",
    ).mkString("\n")

  }

  def movesForPod(pod: Pod, loc: Node, positions: Map[Node, NonEmptyList[Pod]]) = {
    loc match {
      case r @ Room(_) => r.letter != pod.name
      case _           => true
    }
  }.guard[List]
    .as(pod)
    .flatMap { pod =>
      def newEntry(currentContents: NonEmptyList[Pod]): Option[NonEmptyList[Pod]] =
        currentContents.filterNot(_ == pod).toNel

      neighbors(loc)
        .filter {
          // todo: if the target position is a room, we only check if it's the right color.
          case (_, target @ Room(_)) => target.letter == pod.name
          // If it's a corridor, we check that it's empty
          case (_, target @ Corridor(_)) => positions.get(target).isEmpty
        }
    }
    .map(_.swap)
    .toMap

  def moveOnce(
    state: State
  ): immutable.Iterable[State] =
    if (state.cost > minCostSoFar)
      Nil
    else
      state
        .pods
        .toList
        .flatMap { case (loc, pods) =>
          pods.toList.flatMap { pod =>
            def newEntry(currentContents: NonEmptyList[Pod]): Option[NonEmptyList[Pod]] =
              currentContents.filterNot(_ == pod).toNel

            val otherPods = state.copy(pods = state.pods - loc ++ newEntry(pods).tupleLeft(loc))

            movesForPod(pod, loc, state.pods)
              .map { case (nextLoc, dist) =>
                otherPods.copy(
                  pods = otherPods.pods |+| Map(nextLoc -> NonEmptyList.one(pod)),
                  cost = state.cost + dist * pod.multiplier,
                )

              }
          }
        }

  var minCostSoFar = Long.MaxValue

  @tailrec
  final def recurse(state: State, remaining: List[State]): Long = {
    val nextMoves = moveOnce(state)

    println(state.cost)
    if (nextMoves.isEmpty)
      if (state.isFinished) {
        println(minCostSoFar)
        minCostSoFar = minCostSoFar min state.cost
        state.cost
      } else
        Long.MaxValue
    else {
      val l = nextMoves.toList

      recurse(l.head, l.tail ++ remaining)
    }
  }

// moveOnce(initState)

  // println(recurse(initState, Nil))
  println(minCostSoFar)

  assertEquals(
    movesForPod(Pod('A'), rooms(0), Map.empty),
    Map(),
    "No moves for pod in its right place",
  )

  assertEquals(
    movesForPod(Pod('B'), rooms(0), Map.empty),
    Map(corridor(1) -> 2, corridor(2) -> 2),
    "Yes moves for pod in the wrong room",
  )

  assertEquals(
    movesForPod(Pod('B'), corridor(3), Map.empty),
    Map(rooms(1) -> 2, corridor(2) -> 2, corridor(4) -> 2),
    "Yes moves for pod in corridor next to its room",
  )

  assertEquals(
    movesForPod(Pod('B'), corridor(5), Map.empty),
    Map(corridor(4) -> 2, corridor(6) -> 1),
    "Yes moves for pod in corridor next to no useful room",
  )

  assertEquals(
    movesForPod(Pod('B'), corridor(6), Map.empty),
    Map(corridor(5) -> 1),
    "Yes moves for pod in last corridor",
  )

  assertEquals(
    movesForPod(Pod('B'), corridor(6), Map(corridor(5) -> NonEmptyList.of(Pod('C')))),
    Map(),
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
