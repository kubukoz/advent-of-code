package com.kubukoz.adventofcode2017

import scala.annotation.tailrec

object Day19 {

  sealed trait PathElem extends Product with Serializable

  case object Intersection extends PathElem

  case object VerticalLine extends PathElem

  case object HorizontalLine extends PathElem

  case class Letter(char: Char) extends PathElem

  sealed case class Direction(dX: Int, dY: Int) extends Product with Serializable {
    val from: Position => Position = {
      case (x, y) => (x + dX, y + dY)
    }

    def compose(another: Direction): Direction = Direction(dX + another.dX, dY + another.dY)

    def twice: Direction = compose(this)
  }

  object Left extends Direction(-1, 0)

  object Up extends Direction(0, -1)

  object Right extends Direction(1, 0)

  object Down extends Direction(0, 1)

  type Position = (Int, Int)
  val parse: List[String] => Map[Position, PathElem] = _.zipWithIndex.flatMap { case (line, y) =>
    line.zipWithIndex.collect {
      case ('|', x) => (x, y) -> VerticalLine
      case ('-', x) => (x, y) -> HorizontalLine
      case ('+', x) => (x, y) -> Intersection
      case (ch, x) if !ch.isWhitespace => (x, y) -> Letter(ch)
    }
  }.toMap

  def collectLetters(path: Map[Position, PathElem]): (String, Int) = {
    @tailrec
    def go(currentPosition: Position, currentDirection: Direction, mem: List[Char], steps: Int): (List[Char], Int) = {
      val newPosition = currentDirection.from(currentPosition)

      val goingSideways = Set(Left, Right).contains(currentDirection)
      val goingUpDown = Set(Up, Down).contains(currentDirection)

      def findNotEmpty(atDirections: Direction*) = atDirections.find { dir =>
        path.contains(dir.from(newPosition))
      }.get

      path.get(newPosition) match {
        case None => (mem, steps)
        case Some(x) =>
          val newDirection = Some(x).collectFirst {
            case Intersection if goingSideways => findNotEmpty(Up, Down)
            case Intersection if goingUpDown => findNotEmpty(Left, Right)
          }

          val newMem = Some(x).collect { case Letter(ch) => ch :: mem }

          go(newPosition, newDirection.getOrElse(currentDirection), newMem.getOrElse(mem), steps + 1)
      }
    }

    val findTop = path.keys.collectFirst { case pos@(_, 0) => pos }

    val (letters, steps) = go(findTop.get, Down, Nil, 1)

    (letters.reverse.mkString, steps)
  }

  def main(args: Array[String]): Unit = {
    val parsed = parse(fileLines("/day19.txt"))

    println(collectLetters(parsed))
  }
}
