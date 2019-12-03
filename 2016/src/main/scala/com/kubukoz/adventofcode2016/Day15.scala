package com.kubukoz.adventofcode2016

import scala.annotation.tailrec

object Day15 {
  private val discPat = """Disc #. has (\d+) positions; at time=0, it is at position (\d+)\.""".r

  def findFirstHole(input: List[String]): Int = {
    val discs = input.map {
      case discPat(positions, startPosition) => Disc(positions.toInt, startPosition.toInt)
    }

    @tailrec
    def go(state: State, i: Int): Int =
      if (state.canPassThrough) i - 1
      else go(state.next, i + 1)

    go(State(discs), 0)
  }

  case class State(discs: List[Disc]) extends AnyVal {
    def canPassThrough: Boolean = discs match {
      case Disc(_, 0) :: t => State(t).next.canPassThrough
      case Nil => true
      case _ => false
    }

    def next: State = copy(discs.map(_.next))
  }

  case class Disc(positions: Int, currentPosition: Int) {
    def next: Disc = copy(currentPosition = (currentPosition + 1) % positions)
  }

  def main(args: Array[String]): Unit = {
    val input = fileLines("/day15.txt")

    println(findFirstHole(input))
    println(findFirstHole(input :+ "Disc #7 has 11 positions; at time=0, it is at position 0."))
  }
}
