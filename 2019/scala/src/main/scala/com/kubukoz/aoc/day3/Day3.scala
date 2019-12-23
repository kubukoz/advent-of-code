package com.kubukoz.aoc.day3

import cats.effect.Console.io._
import cats.effect.{ExitCode, IO, IOApp}
import cats.implicits._
import cats.kernel.Semigroup
import com.kubukoz.aoc.Util
import com.kubukoz.aoc.day3.data._
import io.chrisdavenport.semigroups.Min

private[day3] object Day3 extends IOApp {

  def parseLine(s: String): PathDescription = {
    val steps = s
      .split(",")
      .map {
        _.splitAt(1) match {
          case (dir, moves) => Move(Direction.fromChar(dir.head), moves.toInt)
        }
      }
      .toList

    PathDescription(steps)
  }

  val startPosition: Point = Point(0, 0)

  def performMove(move: Move, position: Point): Vector[Point] =
    Vector.iterate(position, move.steps + 1)(_ |+| Direction.toDelta(move.direction)).tail

  private def performTraversal(descriptions: List[PathDescription]) = {
    val executePath: PathDescription => List[Point] = _.moves
      .scanLeft(Vector(startPosition))((history, move) => performMove(move, history.last))
      .flatten
      .tail

    descriptions.zipWithIndex.flatMap(_.leftTraverse(executePath(_).zipWithIndex)).foldMap {
      case ((point, indexInPath), pathIndex) => Map(point -> Map(pathIndex -> Min(indexInPath + 1)))
    }
  }

  def part1(descriptions: List[PathDescription]): Option[Int] =
    performTraversal(descriptions).collect {
      case (point, owners) if owners.keys.size > 1 =>
        Point.manhattan(startPosition)(point)
    }.minOption

  def part2(descriptions: List[PathDescription]): Option[Int] =
    performTraversal(descriptions).collect {
      case (_, owners) if owners.keys.size > 1 => owners.values.toList.foldMap(_.getMin)
    }.minOption

  val parse: String => List[PathDescription] = _.linesIterator.map(parseLine).toList

  def run(args: List[String]): IO[ExitCode] =
    for {
      lines <- Util.readFile[IO]("files/day3.txt").map(parse)
      _     <- putStrLn(part1(lines))
      _     <- putStrLn(part2(lines))
    } yield ExitCode.Success
}

private[day3] object data {

  sealed trait Direction extends Product with Serializable

  object Direction {
    case object Up    extends Direction
    case object Left  extends Direction
    case object Right extends Direction
    case object Down  extends Direction

    val fromChar: Char => Direction = {
      case 'U' => Up
      case 'L' => Left
      case 'R' => Right
      case 'D' => Down
    }

    val toDelta: Direction => Point = {
      case Up    => Point(0, 1)
      case Down  => Point(0, -1)
      case Right => Point(1, 0)
      case Left  => Point(-1, 0)
    }
  }

  final case class Move(direction: Direction, steps: Int)
  final case class PathDescription(moves: List[Move]) extends AnyVal

  final case class Point(x: Int, y: Int)

  object Point {

    implicit val semigroup: Semigroup[Point] =
      Semigroup.instance { (p1, p2) =>
        Point(p1.x + p2.x, p1.y + p2.y)
      }

    def manhattan(a: Point)(b: Point): Int =
      (a.x - b.x).abs + (a.y - b.y).abs
  }

}
