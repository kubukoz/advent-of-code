package aoc

import cats.kernel.Monoid

import aoc.lib._
import cats.implicits._
import util.chaining._

object Day20 extends App {

  def parseBits(bits: Seq[Boolean]): Int = Integer.parseInt(
    bits.map {
      if (_)
        1
      else
        0
    }.mkString,
    2,
  )

  case class Position(x: Int, y: Int)

  object Position {
    implicit val monoid: Monoid[Position] = Monoid[(Int, Int)].imap(apply.tupled)(p => (p.x, p.y))
  }

  case class Bounds(from: Position, to: Position) {
    def contains(pos: Position): Boolean =
      (from.x to to.x).contains(pos.x) && (from.y to to.y).contains(pos.y)

    def expandN(n: Int): Bounds = copy(from = from |+| Position(-n, -n), to = to |+| Position(n, n))

    def iterate: List[Position] = iterateAll.flatten

    def iterateAll: List[List[Position]] = (from.y to to.y).toList.map { y =>
      (from.x to to.x).toList.map(Position(_, y))
    }

  }

  case class Image(data: Map[Position, Boolean]) {
    def dropAbsent = copy(data = data.filter { case (_, v) => v })

    def trim(bounds: Bounds) = copy(data = data.filter { case (pos, _) => bounds.contains(pos) })

    def bounds: Bounds = {
      val xs = data.keySet.map(_.x)
      val ys = data.keySet.map(_.y)
      Bounds(
        Position(xs.min, ys.min),
        Position(xs.max, ys.max),
      )
    }

    def render: String = bounds
      .iterateAll
      .map { line =>
        line.map { pos =>
          if (data.getOrElse(pos, false))
            '#'
          else
            '.'
        }.mkString
      }
      .mkString("\n", "\n", "\n")

  }

  def getPoints(
    coords: Position,
    base: Image,
    algorithm: List[Boolean],
    default: Boolean,
  ) =
    coords -> (-1 to 1)
      .flatMap { dy =>
        (-1 to 1).map { dx =>
          Position(dx, dy)
        }
      }
      .map(_ |+| coords)
      .map(p => base.data.getOrElse(p, default))
      .pipe(parseBits)
      .pipe(algorithm)

  def newImage(
    oldImage: Image,
    newBounds: Bounds,
    algorithm: List[Boolean],
    default: Boolean,
  ): Image = newBounds
    .iterate
    .map {
      getPoints(_, oldImage, algorithm, default)
    }
    .toMap
    .pipe(Image(_))

  def parseData(s: String) = s
    .split("\n")
    .zipWithIndex
    .flatMap { case (line, y) =>
      line.toList.zipWithIndex.map { case (ch, x) => Position(x, y) -> (ch == '#') }
    }
    .toMap
    .pipe(Image(_))

  def parse(input: String) =
    input.split("\n\n") match {
      case Array(a, b) =>
        (
          a.split("\n").map(_.trim).mkString.map(_ == '#').toList,
          parseData(b),
        )
    }

  def run(base: Image, algorithm: List[Boolean], totalRounds: Int) =
    (1 to totalRounds)
      .foldLeft(base) { (data, i) =>
        val default = algorithm.head && i % 2 == 0
        newImage(data, base.bounds.expandN(i), algorithm, default = default)
      }
      .trim(base.bounds.expandN(totalRounds))
      .dropAbsent

  locally {
    val (algorithm, data) = parse(readAll("day20-example.txt"))

    assertEquals(run(data, algorithm, 2).data.size, 35, "Part 1")
    assertEquals(run(data, algorithm, 50).data.size, 3351, "Part 2")
  }
  locally {
    val (algorithm, data) = parse(readAll("day20.txt"))

    assertEquals(run(data, algorithm, 2).data.size, 4964, "Part 1")
    assertEquals(run(data, algorithm, 50).data.size, 13202, "Part 2")
  }
}
