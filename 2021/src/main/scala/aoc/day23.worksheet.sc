import cats.data.NonEmptyList

import scala.collection.immutable

import aoc.lib._
import cats.implicits._
/*
#############
#...........#
###C#C#B#D###
  #D#A#B#A#
  #########


 */

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

//completion criteria: no more moves
// todo filter out room->corridor moves as long as the amphipod is in its final room
def moveOnce(state: State): immutable.Iterable[State] = state.pods.flatMap { case (loc, pods) =>
  pods.toList.flatMap { pod =>
    // todo should remove only the current element from the room
    // return None if no more elements
    def newEntry(currentContents: NonEmptyList[Pod]): Option[NonEmptyList[Pod]] = ???

    val otherPods = state.copy(pods = state.pods - loc ++ newEntry(pods).tupleLeft(loc))

    neighbors(loc)
      .filterNot { case (_, nextLoc) =>
        // todo: if the target position is a room, we only check if it's the right color. If it's a corridor, we check that it's empty
        otherPods.pods.contains(nextLoc)
      }
      // filterNot ( isInvalidRoom && nowInCorridor )
      .map { case (dist, nextLoc) =>
        otherPods.copy(
          pods = otherPods.pods |+| Map(nextLoc -> NonEmptyList.one(pod)),
          cost = otherPods.cost + dist * pod.multiplier,
        )
      }

  }
}

moveOnce(initState).size

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
