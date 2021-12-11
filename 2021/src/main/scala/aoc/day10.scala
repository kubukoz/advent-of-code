package aoc

import cats.implicits._

import scala.annotation.tailrec

import lib._
import util.chaining._

object Day10 extends App {

  object data {
    val openers = "(<[{"
    val closers = ")>]}"
    val pairs = (openers.toList, closers.toList).parTupled.toMap
  }

  import data._

  val isOpening = openers.contains(_: Char)
  val isClosing = closers.contains(_: Char)
  def isMatching(closer: Char, opener: Char): Boolean = pairs(opener) == closer

  case class State(
    stack: List[Char],
    remainder: String,
  ) {
    // property test idea: push is the dual of pop
    def push(ch: Char): State = copy(stack = ch :: stack, remainder = remainder.tail)

    def pop: Option[(Char, State)] =
      stack match {
        case head :: rest =>
          (
            head,
            State(rest, remainder.tail),
          ).some

        case Nil => None
      }

    def uncons: Option[(Char, String)] = remainder.headOption.map(ch => (ch, remainder.tail))

    def complete = stack.map(pairs)
  }

  def parse(line: String): Either[Char, State] = {

    @tailrec
    def go(s: State): Either[Char, State] =
      s.uncons match {
        case Some((ch, rest)) if isOpening(ch) => go(s.push(ch))
        case Some((ch, rest)) if isClosing(ch) =>
          s.pop match {
            case Some((top, newState)) if isMatching(closer = ch, opener = top) => go(newState)
            case _                                                              => Left(ch)
          }

        case None => Right(s)
        case _    => throw new Exception("impossible?")
      }

    go(State(Nil, line))
  }

  val part1: List[String] => Int = {
    val points = closers.zip(List(3, 25137, 57, 1197)).toMap

    _.flatMap(parse(_).swap.toOption).map(points).sum
  }

  val part2: List[String] => Long = {
    val points = closers.zip(List(1, 4, 2, 3)).toMap

    def mid[A](l: List[A]): A = l((l.size / 2))

    _.flatMap(parse(_).toOption)
      .map(_.complete.foldLeft(0L)(_ * 5 + points(_)))
      .pipe(_.sorted)
      .pipe(mid)
  }

  val example = readAllLines("day10-example.txt")
  assertEquals(part1(example), 26397, "Part 1 example")
  assertEquals(part2(example), 288957, "Part 2 example")

  val fromFile = readAllLines("day10.txt")
  assertEquals(part1(fromFile), 367059, "Part 1")
  assertEquals(part2(fromFile), 1952146692L, "Part 2")

}
