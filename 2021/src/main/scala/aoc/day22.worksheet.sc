import aoc.lib._

import cats.implicits._

sealed trait Range {

  def intersect(another: Bounds): Range = Intersect(this, another)

  def subtract(another: Bounds): Range =
    this match {
      case Empty => Empty
      case _     => Subtract(this, another)
    }

  def union(another: Bounds): Range =
    this match {
      case Empty => another
      case _     => Union(this, another)
    }

  def size: Long =
    this match {
      case Empty               => 0L
      case Bounds(xs, ys, zs)  => List(xs, ys, zs).map(r => (r.from to r.to).size.toLong).product
      case Union(l, r)         => l.size + r.size - l.intersect(r).size
      case Intersect(Empty, _) => 0L
      case Intersect(l: Bounds, r)   => l.intersectWithBounds(r).fold(0L)(_.size)
      case Intersect(Union(a, b), c) =>
        // size of (a ^ c) v (a ^ b) - distributivity
        a.intersect(c).size + b.intersect(c).size -
          a.intersect(b).intersect(c).size
      case Intersect(Intersect(a, b), c) =>
        // effectively a.intersect(b.intersect(c)) - associativity
        b.intersectWithBounds(c).fold(0L)(a.intersect(_).size)
      case Intersect(Subtract(a, b), c) => a.intersect(c).size - a.intersect(b).intersect(c).size
      case Subtract(a, b)               => a.size - a.intersect(b).size
    }

}

case class Intersect(lhs: Range, rhs: Bounds) extends Range
case class Subtract(lhs: Range, rhs: Bounds) extends Range
case class Union(lhs: Range, rhs: Bounds) extends Range

case class Bounds(xs: BoxRange, ys: BoxRange, zs: BoxRange) extends Range {

  def intersectWithBounds(r: Bounds): Option[Bounds] =
    (
      xs.intersect(r.xs),
      ys.intersect(r.ys),
      zs.intersect(r.zs),
    ).mapN(Bounds.apply)

}

case object Empty extends Range

case class BoxRange(from: Int, to: Int) {
  def contains(point: Int): Boolean = point >= from && point <= to

  def intersect(another: BoxRange): Option[BoxRange] = {
    val newFrom =
      if (another.contains(from))
        from.some
      else if (this.contains(another.from))
        another.from.some
      else
        None

    val newTo =
      if (another.contains(to))
        to.some
      else if (this.contains(another.to))
        another.to.some
      else
        None

    (newFrom, newTo).mapN(BoxRange.apply)
  }

}

object BoxRange {
  def fromStrings(from: String, to: String): BoxRange = BoxRange(from.toInt, to.toInt)
}

case class Instruction(on: Boolean, bounds: Bounds)

def parse(s: List[String]) = s.map { case s"$onOff x=$xFrom..$xTo,y=$yFrom..$yTo,z=$zFrom..$zTo" =>
  val on = onOff == "on"

  Instruction(
    on,
    Bounds(
      xs = BoxRange.fromStrings(xFrom, xTo),
      ys = BoxRange.fromStrings(yFrom, yTo),
      zs = BoxRange.fromStrings(zFrom, zTo),
    ),
  )
}

val instructions = parse(readAllLines("day22.txt"))

val part1Bounds = Bounds(
  BoxRange(-50, 50),
  BoxRange(-50, 50),
  BoxRange(-50, 50),
)

Empty.size

part1Bounds.size

val r =
  instructions
    .foldLeft(Empty: Range) { (range, ins) =>
      if (ins.on)
        range.union(ins.bounds)
      else
        range.subtract(ins.bounds)
    }
// .intersect(part1Bounds)
    .size

instructions.size

2758514936282235L
