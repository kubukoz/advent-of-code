import aoc.lib._

import cats.implicits._

case class Position(x: Int, y: Int, z: Int)

sealed trait Range

case class BoxRange(from: Int, to: Int) extends Range {
  def toList: List[Int] = (from to to).toList
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

case class Bounds(xs: BoxRange, ys: BoxRange, zs: BoxRange) {

  def intersect(another: Bounds): Option[Bounds] =
    (
      xs.intersect(another.xs),
      ys.intersect(another.ys),
      zs.intersect(another.zs),
    ).mapN(Bounds.apply)

  def toSet: Set[Position] = (xs.toList, ys.toList, zs.toList).mapN(Position.apply).toSet
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

val init: Set[Position] = Set.empty

// instructions
//   .foldLeft(init) { (s, ins) =>
//     val relevant = ins.bounds.intersect(part1Bounds).foldMap(_.toSet)
//     if (ins.on)
//       s ++ relevant
//     else
//       s -- relevant

//   }
//   .size

instructions.size
