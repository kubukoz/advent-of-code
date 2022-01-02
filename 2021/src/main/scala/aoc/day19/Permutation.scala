package aoc.day19

import cats.implicits._
import cats.kernel.CommutativeGroup
import cats.kernel.Monoid
import cats.kernel.Order

case class Position(x: Int, y: Int, z: Int) {

  def distance(another: Position): Int = List(
    x - another.x,
    y - another.y,
    z - another.z,
  ).foldMap(_.abs)

}

object Position {
  implicit val ord: Order[Position] = Order.by(p => (p.x, p.y, p.z))
  implicit val group: CommutativeGroup[Position] =
    CommutativeGroup[(Int, Int, Int)].imap(apply.tupled)(pos => (pos.x, pos.y, pos.z))

  val init = group.empty
}

sealed trait AxisKind

object AxisKind {
  case object X extends AxisKind
  case object Y extends AxisKind
  case object Z extends AxisKind
}

case class Axis(kind: AxisKind, sign: Int) {
  def unary_- : Axis = copy(sign = sign * -1)

  def compile: Position => Int =
    kind match {
      case AxisKind.X => _.x * sign
      case AxisKind.Y => _.y * sign
      case AxisKind.Z => _.z * sign
    }

}

object Axis {

  val X = Axis(AxisKind.X, sign = 1)
  val Y = Axis(AxisKind.Y, sign = 1)
  val Z = Axis(AxisKind.Z, sign = 1)

}

case class Permutation(x: Axis, y: Axis, z: Axis) {

  def get(axis: Axis): Axis = {
    val newAxis = {
      axis.kind match {
        case AxisKind.X => x
        case AxisKind.Y => y
        case AxisKind.Z => z
      }
    }

    newAxis.copy(sign = newAxis.sign * axis.sign)
  }

  def compile: Position => Position = (x.compile, y.compile, z.compile).mapN(Position.apply)

}

object Permutation {
  import Axis._

  val id: Permutation = Permutation(X, Y, Z)

  implicit val monoid: Monoid[Permutation] =
    new Monoid[Permutation] {

      def combine(a: Permutation, b: Permutation): Permutation = Permutation(
        x = a.get(b.x),
        y = a.get(b.y),
        z = a.get(b.z),
      )

      val empty: Permutation = id

    }

  val all: List[Permutation] = {

    val byX = List(
      Permutation.id,
      Permutation(X, -Z, Y),
      Permutation(X, -Y, -Z),
      Permutation(X, Z, -Y),
    )

    val byY = List(
      Permutation.id,
      Permutation(-Z, Y, X),
      Permutation(-X, Y, -Z),
      Permutation(Z, Y, -X),
    )

    val byZ = List(
      Permutation.id,
      Permutation(-Y, X, Z),
      Permutation(-X, -Y, Z),
      Permutation(Y, -X, Z),
    )

    (byX, byY, byZ).mapN(_ |+| _ |+| _).distinct
  }

  val allCompiled: List[Position => Position] = all.map(_.compile)

}
