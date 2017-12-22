package com.kubukoz.adventofcode2017

import com.softwaremill.quicklens._
import enumeratum._

import scala.annotation.tailrec
import scala.collection.immutable

object Day22 {

  sealed case class Direction(dX: Int, dY: Int) extends EnumEntry {
    val from: Coords => Coords = {
      _.modify(_.x).using(_ + dX).modify(_.y).using(_ + dY)
    }

    lazy val next = Day22.next(this)
    lazy val previous =
      if (this == Direction.Left) Direction.Down
      else Direction.values(Direction.indexOf(this) - 1)
  }

  object Direction extends Enum[Direction] {
    object Left extends Direction(-1, 0)
    object Up extends Direction(0, -1)
    object Right extends Direction(1, 0)
    object Down extends Direction(0, 1)

    override def values: immutable.IndexedSeq[Direction] = findValues
  }

  sealed trait InfectionState extends EnumEntry

  object InfectionState extends Enum[InfectionState] {
    case object Clean extends InfectionState
    case object Weakened extends InfectionState
    case object Infected extends InfectionState
    case object Flagged extends InfectionState

    override def values: immutable.IndexedSeq[InfectionState] = findValues
  }

  case class Coords(x: Long, y: Long)

  def next[T <: EnumEntry](value: T)(implicit enum: Enum[T]): T = {
    enum.values((enum.indexOf(value) + 1) % enum.values.size)
  }

  import InfectionState._

  case class Carrier(position: Coords, direction: Direction)

  case class State(infected: Map[Coords, InfectionState],
                   carrier: Carrier,
                   gotInfected: Boolean)

  def parseMap(strings: List[String]): Map[Coords, InfectionState] = {
    val width = strings.head.length
    val height = strings.size

    val dx = (width - 1) / 2
    val dy = (height - 1) / 2

    strings.zipWithIndex
      .flatMap {
        case (line, y) =>
          line.zipWithIndex.collect { case ('#', x) => Coords(x - dx, y - dy) -> Infected }
      }.toMap.withDefaultValue(Clean)
  }

  def burst(state: State,
            changeState: InfectionState => InfectionState): State = {

    val position = state.carrier.position
    val currentState = state.infected(position)

    val newState = changeState(currentState)
    val newDirection = {
      val oldDirection = state.carrier.direction
      currentState match {
        case Clean    => oldDirection.previous
        case Weakened => oldDirection
        case Infected => oldDirection.next
        case Flagged  => oldDirection.next.next
      }
    }

    state
      .modify(_.infected).using(_ + (position -> newState))
      .modify(_.carrier.direction).setTo(newDirection)
      .modify(_.carrier.position).using(newDirection.from)
      .modify(_.gotInfected).setTo(newState == Infected)
  }

  def transform(changeState: InfectionState => InfectionState)(
      input: Map[Coords, InfectionState],
      rounds: Int): Int = {

    val init = State(
      input,
      Carrier(Coords(0, 0), Direction.Up),
      gotInfected = false
    )

    (1 to rounds).foldLeft((init, 0)) { case ((value, infected), _) =>
      val result = burst(value, changeState)
      (result, if (result.gotInfected) infected + 1 else infected)
    }._2
  }

  val part1: (Map[Coords, InfectionState], Int) => Int = transform {
    case Infected => Clean
    case Clean    => Infected
    case _        => sys.error("impossible")
  }

  val part2: (Map[Coords, InfectionState], Int) => Int = transform(next(_))

  def main(args: Array[String]): Unit = {
    val input = parseMap(fileLines("/day22.txt"))

    println(part1(input, 10000))
    println(part2(input, 10000000))
  }
}
