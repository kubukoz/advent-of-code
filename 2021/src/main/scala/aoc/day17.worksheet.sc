import cats.data

import cats.kernel.Monoid

import aoc.lib._
import cats.implicits._
val example = readAll("day17-example.txt")
val fromFile = readAll("day17.txt")

val input = example

case class Position(
  x: Int,
  y: Int,
)

case class Area(from: Position, to: Position) {

  def contains(point: Position): Boolean =
    (from.x to to.x).contains(point.x) &&
      (from.y to to.y).contains(point.y)

}

object Position {
  implicit val monoid: Monoid[Position] =
    Monoid[(Int, Int)].imap((apply _).tupled)(pos => (pos.x, pos.y))
}

val init = Position(0, 0)

case class State(position: Position, velocity: Position)

object State {
  implicit val monoid: Monoid[State] =
    Monoid[(Position, Position)].imap((apply _).tupled)(s => (s.position, s.velocity))
}

def performStep(state: State): State =
  state |+| State(
    position = state.velocity,
    // vx = initvx - steps*sign(initvx)
    // vy = initvx-steps
    velocity = Position(x = -state.velocity.x.sign, y = -1),
  )

//returns true if the point can theoretically approaches the area with the current velocity
def canReach(position: Position, targetArea: Area, velocity: Position): Boolean =
  // distance between position and area becomes smaller on the next move
  // if the point is on the left of the area and going right
  // or it's on the right and going left
  // same for top/below
  {
    println(position)
    targetArea.contains(position) || false
  }

def matches(
  velocity: Position,
  targetArea: Area,
): Boolean = LazyList
  .iterate(State(init, velocity))(performStep)
  .takeWhile { s =>
    canReach(s.position, targetArea, velocity = velocity)
  }
  .exists(s => targetArea.contains(s.position))

val targetArea =
  input match {
    case s"target area: x=$xFrom..$xTo, y=$yFrom..$yTo" =>
      Area(Position(xFrom.toInt, yFrom.toInt), Position(xTo.toInt, yTo.toInt))
  }

targetArea

matches(
  velocity = Position(9, 0),
  targetArea = targetArea,
)
