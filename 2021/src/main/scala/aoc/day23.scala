package aoc

import cats.implicits._

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
  val initStateBase = State(
    Set(
      rooms(0) -> Pod('B'),
      rooms(1) -> Pod('C'),
      rooms(1) -> Pod('D'),
      rooms(2) -> Pod('B'),
      rooms(3) -> Pod('A'),
    ),
    0L,
  )

  val initState = State(
    Set(
      rooms(3) -> Pod('B'),
      rooms(2) -> Pod('B'),
      rooms(1) -> Pod('D'),
    ),
    0L,
  )
  // val initState = initStateBase

  case class State(pending: Set[(Node, Pod)], totalCost: Long) {
    def isFinished = pending.isEmpty
  }

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

  var minCostSoFar = Long.MaxValue

  /*
  TODO: implement these

  Amphipods will never move from the hallway into a room unless that room is their destination room and that room contains no amphipods which do not also have that room as their own destination.
  If an amphipod's starting room is not its destination room, it can stay in that room until it leaves the room.
  (For example, an Amber amphipod will not move from the hallway into the right three rooms, and will only move into the leftmost room if that room is empty or if it only contains other Amber amphipods.)

  Once an amphipod stops moving in the hallway, it will stay in that spot until it can move into a room.
  (That is, once any amphipod starts moving, any other amphipods currently in the hallway are locked in place and will not move again until they can move fully into a room.)
   */
  def moveOnce(state: State): List[State] = {
    if (minCostSoFar < state.totalCost)
      Nil

    state.pending.toList.flatMap { case (currentLoc, pod) =>
      val result = movesForPod(pod, currentLoc, state.pending.map(_._1))

      result match {
        case Finished(distance) =>
          state.copy(
            pending = state.pending - ((currentLoc, pod)),
            totalCost = state.totalCost + pod.multiplier * distance,
          ) :: Nil
        case ToCorridors(possibleTargets) =>
          possibleTargets.map { case (target, distance) =>
            state.copy(
              pending = state.pending - ((currentLoc, pod)) + ((target, pod)),
              totalCost = state.totalCost + pod.multiplier * distance,
            )
          }
      }
    }
  }

  object Done

  case class Frame(current: State, history: Set[Set[(Node, Pod)]])

  def shortestDistance(frame: Frame, memory: List[Frame]): Done.type =
    if (frame.current.isFinished) {
      val newCost = minCostSoFar min frame.current.totalCost

      if (newCost < minCostSoFar)
        println("new record! " + newCost)
      minCostSoFar = newCost
      memory match {
        case Nil       => Done
        case h :: more => shortestDistance(h, more)
      }
    } else {

      moveOnce(frame.current).filterNot(it => frame.history.contains(it.pending)) match {
        case Nil =>
          memory match {
            case Nil       => Done
            case h :: more => shortestDistance(h, more)
          }
        case h :: more =>
          val newChild = {
            val base = frame.history + frame.current.pending

            Frame(_, base)
          }

          shortestDistance(newChild(h), more.map(newChild) ++ memory)
      }
    }

  println(shortestDistance(Frame(initState, Set.empty), Nil))
  println(minCostSoFar)

  assertEquals(
    moveOnce(initState).size,
    10,
    "10 moves initially",
  )

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
