import cats.data

import cats.kernel.Monoid

import aoc.lib._
import cats.implicits._
val example = readAll("day17-example.txt")
val fromFile = readAll("day17.txt")

// val input = example
val input = fromFile

case class Position(
  x: Int,
  y: Int,
)

case class Area(from: Position, to: Position) {

  def contains(point: Position): Boolean =
    // todo ordering of this
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
    velocity = Position(x = -state.velocity.x.sign, y = -1),
  )

//returns true if the point can theoretically move again and not get away from the area
// todo: assumes targetx>0 targety<0
def canReach(position: Position, targetArea: Area): Boolean =
  position.x <= targetArea.to.x && position.y >= targetArea.from.y

def buildPath(velocity: Position) = LazyList
  .iterate(State(init, velocity))(performStep)
  .takeWhile { s =>
    canReach(s.position, targetArea)
  }

def matches(
  velocity: Position,
  targetArea: Area,
): Boolean = buildPath(velocity).exists(s => targetArea.contains(s.position))

val targetArea =
  input match {
    case s"target area: x=$xFrom..$xTo, y=$yFrom..$yTo" =>
      // todo range ordering might be different
      Area(Position(xFrom.toInt, yFrom.toInt), Position(xTo.toInt, yTo.toInt))
  }

targetArea

matches(
  velocity = Position(0, 0),
  targetArea = targetArea,
)

val validVelocities = ((1 to targetArea.to.x).toList, targetArea.from.y to (-targetArea.from.y))
  .mapN(Position.apply)
validVelocities.size

val paths = validVelocities.map(buildPath)

val matchingVelocities = paths.filter { v =>
  v.map(_.position).exists(targetArea.contains)
}

matchingVelocities.size

matchingVelocities
  .maxBy(_.map(_.position.y).max)
  .map(_.position.y)
  .max

matchingVelocities.size
//8256
