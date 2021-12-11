import cats.data.State

import aoc.lib._

import cats.implicits._
val example = readAllLines("day10-example.txt")
val fromFile = readAllLines("day10.txt")

val input = fromFile

case class Result()

case class Failure(char: Char)

case class State(
  // head - most recent
  stack: List[Char],
  remainder: String,
) {
  def push(ch: Char): State = copy(stack = ch :: stack, remainder = remainder.tail)

  def pop: Option[(Char, State)] =
    stack match {
      case head :: rest => (head, State(stack = rest, remainder = remainder.tail)).some
      case Nil          => None
    }

  def uncons: Option[(Char, String)] = remainder.headOption.map(ch => (ch, remainder.tail))

  def complete = stack.map(Opener.pairs)
}

object Opener {

  val openers = "(<[{"
  def unapply(ch: Char): Option[Char] = openers.find(ch == _)

  val pairs = (openers.toList, Closer.closers.toList).parTupled.toMap
}

object Closer {
  val closers = ")>]}"
  def unapply(ch: Char): Option[Char] = closers.find(ch == _)

  def matching(closer: Char, opener: Char): Boolean =
    closers.indexOf(closer) == Opener.openers.indexOf(opener)
}

def parse(s: State): Either[Failure, State] =
  s.uncons match {
    case None => Right(s)
    case Some((Closer(ch), rest)) =>
      s.pop match {
        case Some((top, newState)) if Closer.matching(ch, top) => parse(newState)
        case _ => /* didn't expect close, or we got the wrong closer */ Left(Failure(ch))
      }

    case Some((Opener(ch), rest)) => parse(s.push(ch))
    case _                        => throw new Exception("impossible?")
  }

def parseFull(line: String) = parse(State(Nil, line))

val points = Map(
  ')' -> 3,
  ']' -> 57,
  '}' -> 1197,
  '>' -> 25137,
)

// input.flatMap { line =>
//   parseFull(line).swap.toOption.map(_.char).map(points)
// }.sum

val completePoints = Map(
  ')' -> 1,
  ']' -> 2,
  '}' -> 3,
  '>' -> 4,
)

def mid[A](l: List[A]): A = l((l.size / 2))
import util.chaining._

val s = input
  .flatMap { line =>
    parseFull(line)
      .toOption
      .map(
        _.complete.foldLeft(0L) { (sum, next) =>
          (sum * 5) + completePoints(next)
        }
      )
  }
  .pipe(_.sorted)
  .pipe(mid)
