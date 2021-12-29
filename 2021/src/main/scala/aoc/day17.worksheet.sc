import cats.kernel.Monoid

import aoc.lib._
import cats.implicits._

val example = readAll("day17-example.txt")
val fromFile = readAll("day17.txt")

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
    velocity = Position(x = -state.velocity.x.sign, y = -1),
  )

//returns true if the point can theoretically move again and not get away from the area
// assumes targetx>0 targety<0
def canReach(position: Position, targetArea: Area): Boolean = {
  require(targetArea.to.x > 0, "target x must be > 0")
  require(targetArea.from.y < 0, "target y must be < 0")
  position.x <= targetArea.to.x && position.y >= targetArea.from.y
}

def buildPath(velocity: Position, targetArea: Area) = LazyList
  .iterate(State(init, velocity))(performStep)
  .takeWhile { s =>
    canReach(s.position, targetArea)
  }

def matches(
  velocity: Position,
  targetArea: Area,
): Boolean = buildPath(velocity, targetArea).exists(s => targetArea.contains(s.position))

// val input = example
val input = fromFile

val targetArea =
  input match {
    case s"target area: x=$xFrom..$xTo, y=$yFrom..$yTo" =>
      // todo range ordering might be different
      Area(Position(xFrom.toInt, yFrom.toInt), Position(xTo.toInt, yTo.toInt))
  }

val validVelocities = ((1 to targetArea.to.x).toList, targetArea.from.y to (-targetArea.from.y))
  .mapN(Position.apply)

val paths = validVelocities.map(buildPath(_, targetArea))

val matchingVelocities = paths.filter { v =>
  v.map(_.position).exists(targetArea.contains)
}

matchingVelocities
  .maxBy(_.map(_.position.y).max)
  .map(_.position.y)
  .max

matchingVelocities.size
