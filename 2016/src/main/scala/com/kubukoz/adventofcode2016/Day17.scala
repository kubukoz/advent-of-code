package com.kubukoz.adventofcode2016

import com.kubukoz.adventofcode2016.Commons.{md5Separated => md5}

import scala.annotation.tailrec
import scala.language.postfixOps

object Day17 {
  private val mapBounds = (0 to 3).toSet

  case class Position(pos: (Int, Int)) extends AnyVal {
    def x: Int = pos._1

    def y: Int = pos._2

    def fitsBounds: Boolean = {
      Set(x, y).forall(mapBounds contains)
    }
  }

  case class State(position: Position, history: String) {
    def isComplete: Boolean = position == Position(3, 3)

    val moves = List((0, -1), (0, 1), (-1, 0), (1, 0))

    def maybeGo(moveIndex: Int, charAtHash: Char): Option[State] = {
      val (dx, dy) = moves(moveIndex)
      val newPosition = Position(position.x + dx, position.y + dy)

      //noinspection SimplifyBooleanMatch because it ignores guards
      newPosition.fitsBounds match {
        case true if 'B' to 'F' contains charAtHash =>
          Some(State(newPosition, history + "UDLR".apply(moveIndex)))
        case _ => None
      }
    }
  }

  private def nextPossibilities(input: String, state: State): Set[State] = {
    val hash = md5(input + state.history)
    for {
      move <- mapBounds
      newState <- state.maybeGo(move, hash.charAt(move))
    } yield newState
  }

  def findShortestPath(input: String): String = {
    @tailrec
    def goRec(possibilities: Set[State], depth: Int): String = {
      if (possibilities.exists(_.isComplete)) possibilities.filter(_.isComplete).head.history
      else goRec(possibilities.flatMap(nextPossibilities(input, _)), depth + 1)
    }

    goRec(Set(State(Position(0, 0), "")), 0)
  }

  def findLongestPath(input: String): Int = {
    def goRec(possibilities: Set[State], depth: Int): Int = {
      val (complete, incomplete) = possibilities.partition(_.isComplete)

      val completeMax = if (complete.nonEmpty) depth else 0
      val incompleteMax = if (incomplete.isEmpty) 0 else goRec(incomplete.flatMap(nextPossibilities(input, _)), depth + 1)

      completeMax max incompleteMax
    }

    goRec(Set(State(Position(0, 0), "")), 0)
  }

  def main(args: Array[String]): Unit = {
    val input = "mmsxrhfx"

    println(findShortestPath(input))
    println(findLongestPath(input))
  }
}
