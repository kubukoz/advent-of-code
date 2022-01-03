package aoc

import aoc.lib._
import cats.implicits._

object Day22 extends App {

  sealed trait Range {

    def intersect(another: Area): Range = Intersect(this, another)

    def subtract(another: Area): Range = Subtract(this, another)

    def union(another: Area): Range = Union(this, another)

    def size: Long =
      this match {
        case Empty            => 0L
        case Area(xs, ys, zs) => List(xs, ys, zs).map(r => (r.from to r.to).size.toLong).product
        case Union(l, r)      => l.size + r.size - l.intersect(r).size
        case Subtract(a, b)   => a.size - a.intersect(b).size

        case Intersect(lhs, bounds) =>
          lhs match {
            case Empty       => 0L
            case l: Area     => l.intersectWithBounds(bounds).fold(0L)(_.size)
            case Union(a, b) =>
              // transform to (a ∩ c) ∪ (a ∩ b) - distributivity
              a.intersect(bounds).size + b.intersect(bounds).size -
                a.intersect(b).intersect(bounds).size
            case Intersect(a, b) =>
              // transform to a ∩ (b ∩ c) - associativity
              b.intersectWithBounds(bounds).fold(0L)(a.intersect(_).size)
            case Subtract(a, b) => a.intersect(bounds).size - a.intersect(b).intersect(bounds).size
          }
      }

  }

  case class Intersect(lhs: Range, rhs: Area) extends Range
  case class Subtract(lhs: Range, rhs: Area) extends Range
  case class Union(lhs: Range, rhs: Area) extends Range

  case class Area(xs: BoxRange, ys: BoxRange, zs: BoxRange) extends Range {

    def intersectWithBounds(r: Area): Option[Area] =
      (
        xs.intersect(r.xs),
        ys.intersect(r.ys),
        zs.intersect(r.zs),
      ).mapN(Area.apply)

  }

  case object Empty extends Range

  case class BoxRange(from: Int, to: Int) {
    def contains(point: Int): Boolean = point >= from && point <= to

    def intersect(another: BoxRange): Option[BoxRange] = {
      def choosePoint(f: BoxRange => Int) = f(this)
        .some
        .filter(another.contains)
        .orElse(f(another).some.filter(this.contains))

      (
        choosePoint(_.from),
        choosePoint(_.to),
      ).mapN(BoxRange.apply)
    }

  }

  object BoxRange {
    def fromStrings(from: String, to: String): BoxRange = BoxRange(from.toInt, to.toInt)
  }

  case class Instruction(on: Boolean, bounds: Area)

  def parse(
    s: List[String]
  ) = s.map { case s"$onOff x=$xFrom..$xTo,y=$yFrom..$yTo,z=$zFrom..$zTo" =>
    val on = onOff == "on"

    Instruction(
      on,
      Area(
        xs = BoxRange.fromStrings(xFrom, xTo),
        ys = BoxRange.fromStrings(yFrom, yTo),
        zs = BoxRange.fromStrings(zFrom, zTo),
      ),
    )
  }

  def runAll(instructions: List[Instruction]) =
    instructions
      .foldLeft(Empty: Range) { (range, ins) =>
        if (ins.on)
          range.union(ins.bounds)
        else
          range.subtract(ins.bounds)
      }

  def part1(instructions: List[Instruction]) = {

    val bounds = Area(
      BoxRange(-50, 50),
      BoxRange(-50, 50),
      BoxRange(-50, 50),
    )

    runAll(instructions).intersect(bounds).size
  }

  def part2(instructions: List[Instruction]) = runAll(instructions).size

  locally {
    val input = readAllLines("day22-example.txt")

    assertEquals(part1(parse(input)), 474140L, "Part 1 example")
    assertEquals(part2(parse(input)), 2758514936282235L, "Part 2 example")
  }

  locally {
    val input = readAllLines("day22.txt")

    assertEquals(part1(parse(input)), 603661L, "Part 1")
    assertEquals(part2(parse(input)), 1237264238382479L, "Part 2")
  }
}
