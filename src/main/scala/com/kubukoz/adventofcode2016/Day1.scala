package com.kubukoz.adventofcode2016

object Day1 {
  def main(args: Array[String]): Unit = {
    val input = "R3, R1, R4, L4, R3, R1, R1, L3, L5, L5, L3, R1, R4, L2, L1, R3, L3, R2, R1, R1, L5, L2, L1, R2, L4, R1, L2, L4, R2, R2, L2, L4, L3, R1, R4, R3, L1, R1, L5, R4, L2, R185, L2, R4, R49, L3, L4, R5, R1, R1, L1, L1, R2, L1, L4, R4, R5, R4, L3, L5, R1, R71, L1, R1, R186, L5, L2, R5, R4, R1, L5, L2, R3, R2, R5, R5, R4, R1, R4, R2, L1, R4, L1, L4, L5, L4, R4, R5, R1, L2, L4, L1, L5, L3, L5, R2, L5, R4, L4, R3, R3, R1, R4, L1, L2, R2, L1, R4, R2, R2, R5, R2, R5, L1, R1, L4, R5, R4, R2, R4, L5, R3, R2, R5, R3, L3, L5, L4, L3, L2, L2, R3, R2, L1, L1, L5, R1, L3, R3, R4, R5, L3, L5, R1, L3, L5, L5, L2, R1, L3, L1, L3, R4, L1, R3, L2, L2, R3, R3, R4, R4, R1, L4, R1, L5"
    println(findDistanceToLast(input))
    println(findDistanceToFirstRepeated(input))
  }

  private def findAllPositions(startState: State, input: String): Array[Position] = {
    val pattern = """([RL])(\d+)""".r

    val moves = input.split(", ").map {
      case pattern("R", steps) => Move(Right, steps.toInt)
      case pattern("L", steps) => Move(Left, steps.toInt)
    }.flatMap {
      case Move(direction, steps) if steps > 0 =>
        Move(direction, 1) :: List.fill(steps - 1)(Move(NoTurn, 1))
    }

    moves.scanLeft(startState)(_ moveBy _).map(_.position)
  }

  def findDistanceToLast(input: String): Int = {
    val startPosition = Position(0, 0)
    val endPosition = findAllPositions(State(startPosition, Direction(0, 1)), input).last

    startPosition.distanceTo(endPosition)
  }

  def findDistanceToFirstRepeated(input: String): Int = {
    val startPosition = Position(0, 0)
    val allPositions = findAllPositions(State(startPosition, Direction(0, 1)), input)

    //don't do this in production code :D
    val positionsWithAppearances: Map[Position, Array[Int]] = allPositions.zipWithIndex
      .groupBy(_._1).mapValues(_.map(_._2))

    val positionsAppearingTwiceOrMore = positionsWithAppearances.filter(_._2.length >= 2)

    val (firstDoubledPosition, _) = positionsAppearingTwiceOrMore.minBy { case (_, Array(_, app2, _*)) => app2 }

    startPosition.distanceTo(firstDoubledPosition)
  }

  case class Direction(dx: Int, dy: Int) {
    def rotate(turn: Turn) = turn match {
      case Right => copy(-dy, dx)
      case Left => copy(dy, -dx)
      case NoTurn => this
    }
  }

  case class State(position: Position, direction: Direction) {
    def moveBy(move: Move): State = {
      val newDirection = direction.rotate(move.turn)

      val newPosition = position.copy(
        x = position.x + newDirection.dx * move.steps,
        y = position.y + newDirection.dy * move.steps
      )

      State(newPosition, newDirection)
    }
  }

  case class Move(turn: Turn, steps: Int)

  case class Position(x: Int, y: Int) {
    def distanceTo(another: Position): Int = math.abs(x - another.x) + math.abs(y - another.y)
  }

  sealed trait Turn

  case object Left extends Turn

  case object NoTurn extends Turn

  case object Right extends Turn
}
