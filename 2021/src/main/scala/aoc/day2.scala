package aoc

import cats.data.State

import cats.implicits._

object Day2 extends App {

  sealed trait Command

  object Command {
    final case class Up(n: Int) extends Command
    final case class Down(n: Int) extends Command
    final case class Forward(n: Int) extends Command
  }

  final case class AppState(x: Int, y: Int, aim: Int) {
    def result = x * y
  }

  type S[A] = State[AppState, A]

  import Command._

  val parse: String => Command = {
    case s"up $n"      => Up(n.toInt)
    case s"down $n"    => Down(n.toInt)
    case s"forward $n" => Forward(n.toInt)
  }

  val modPart1: Command => S[Unit] = {
    case Up(n)      => State.modify(s => s.copy(y = s.y - n))
    case Down(n)    => State.modify(s => s.copy(y = s.y + n))
    case Forward(n) => State.modify(s => s.copy(x = s.x + n))
  }

  val modPart2: Command => S[Unit] = {
    case Down(n)    => State.modify(s => s.copy(aim = s.aim + n))
    case Up(n)      => State.modify(s => s.copy(aim = s.aim - n))
    case Forward(n) => State.modify(s => s.copy(x = s.x + n, y = s.y + (s.aim * n)))
  }

  val data = lib.readAllLines("day2.txt")

  def solve(mod: Command => S[Unit]): Int =
    data
      .map(parse)
      .traverse(mod)
      .runS(AppState(0, 0, 0))
      .value
      .result

  println(s"Part 1: ${solve(modPart1)}")
  println(s"Part 1: ${solve(modPart2)}")

}
