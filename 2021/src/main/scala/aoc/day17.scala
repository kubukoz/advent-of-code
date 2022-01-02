package aoc

import cats.kernel.Monoid

import aoc.lib._
import cats.implicits._

object Day17 extends App {

  case class Position(x: Int, y: Int)

  case class Area(from: Position, to: Position) {

    def contains(point: Position): Boolean =
      (from.x to to.x).contains(point.x) &&
        (from.y to to.y).contains(point.y)

  }

  object Position {
    implicit val monoid: Monoid[Position] =
      Monoid[(Int, Int)].imap(apply.tupled)(pos => (pos.x, pos.y))

    val init = Position(0, 0)
  }

  case class State(position: Position, velocity: Position)

  object State {
    implicit val monoid: Monoid[State] =
      Monoid[(Position, Position)].imap(apply.tupled)(s => (s.position, s.velocity))
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
    .iterate(State(Position.init, velocity))(performStep)
    .takeWhile { s =>
      canReach(s.position, targetArea)
    }

  def matches(
    velocity: Position,
    targetArea: Area,
  ): Boolean = buildPath(velocity, targetArea).exists(s => targetArea.contains(s.position))

  def findPaths(targetArea: Area): Seq[LazyList[State]] = {
    val validVelocities =
      (
        // vx must be >=1 to even move from (0, 0)
        // at least one step needs to result in x <= targetArea.to.x
        (1 to targetArea.to.x).toList,
        // if vy <= 0: analogous to vx: at least one step needs to result in y >= targetArea.from.y (lowest point), so <targetArea.from.y, 0>
        // if vy > 0: once the points start dropping in "vy" steps, another "vy" steps will result in y=0 again.
        // Then the next point lands at (..., -vy)
        // so -vy needs to be in range of <targetArea.from.y, -1>. Flip the signs, vy needs to be in <1, -targetArea.from.y>.
        // Combine the two ranges to get <tfy, -tfy>
        targetArea.from.y to (-targetArea.from.y),
      )
        .mapN(Position.apply)

    validVelocities
      .map(buildPath(_, targetArea))
      .filter(_.map(_.position).exists(targetArea.contains))
  }

  val parse: String => Area = { case s"target area: x=$xFrom..$xTo, y=$yFrom..$yTo" =>
    Area(Position(xFrom.toInt, yFrom.toInt), Position(xTo.toInt, yTo.toInt))
  }

  val solve = parse.andThen(findPaths)

  def part1(solution: Seq[LazyList[State]]) =
    solution
      .map(_.map(_.position.y).max)
      .max

  def part2(solution: Seq[Any]) = solution.size

  locally {
    lazy val paths = solve(readAll("day17-example.txt"))

    assertEquals(part1(paths), 45, "Part 1 example")
    assertEquals(part2(paths), 112, "Part 2 example")
  }

  locally {
    lazy val paths = solve(readAll("day17.txt"))

    assertEquals(part1(paths), 8256, "Part 1")
    assertEquals(part2(paths), 2326, "Part 2")
  }

}
