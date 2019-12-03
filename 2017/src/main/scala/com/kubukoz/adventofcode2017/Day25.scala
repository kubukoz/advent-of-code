package com.kubukoz.adventofcode2017

import fastparse.WhitespaceApi

import scala.annotation.tailrec

object Day25 {
  private val White = WhitespaceApi.Wrapper {
    import fastparse.all._
    NoTrace(" ".rep)
  }

  import White._
  import fastparse.noApi._

  sealed case class Direction(dX: Int)
  object Left extends Direction(-1)
  object Right extends Direction(1)

  case class Desc(writeValue: Boolean, direction: Direction, newState: Char)
  case class App(startState: Char,
                 steps: Int,
                 states: Map[Char, Map[Boolean, Desc]])

  val appParser: Parser[App] = {
    val number: P[Int] = P(CharIn('0' to '9').rep(1).!.map(_.toInt))
    val bit: P[Boolean] = P("1".!.map(_ => true) | "0".!.map(_ => false))

    val direction: P[Direction] = P(
      "left".!.map(_ => Left) | "right".!.map(_ => Right)
    )

    val descParser: P[(Boolean, Desc)] = P(
      "If the current value is " ~ bit ~ ":\n"
        ~ (
          "- Write the value " ~ bit ~ ".\n" ~
            "- Move one slot to the " ~ direction ~ ".\n" ~
            "- Continue with state " ~ AnyChar.!.map(_.head) ~ "."
        ).map(Desc.tupled)
    )

    val stateParser: Parser[(Char, Map[Boolean, Desc])] = P(
      "In state" ~ AnyChar.!.map(_.head) ~ ":\n" ~
        descParser.rep(1, "\n").map(_.toMap)
    )

    P(
      "Begin in state " ~ AnyChar.!.map(_.head) ~ ".\n" ~
        "Perform a diagnostic checksum after " ~ number ~ "steps.\n\n" ~
        stateParser.rep(1, "\n\n").map(_.toMap) ~ End
    ).map(App.tupled)
  }

  case class CurrentState(name: Char, index: Int, tape: Vector[Boolean])

  def run(app: App): Int = {
    @tailrec
    def go(stepsLeft: Int,
           state: Char,
           index: Int,
           tape: Vector[Boolean]): Vector[Boolean] = {
      stepsLeft match {
        case 0 => tape
        case _ =>
          val atPreviousIndex = tape.drop(index).headOption
          val nextStep = app.states(state)(atPreviousIndex.getOrElse(false))

          val wouldBeIndex = index + nextStep.direction.dX

          val newTape = {
            if (atPreviousIndex.isEmpty) tape :+ nextStep.writeValue
            else if (wouldBeIndex < 0)
              false +: tape.updated(0, nextStep.writeValue)
            else tape.updated(index, nextStep.writeValue)
          }

          go(stepsLeft - 1, nextStep.newState, wouldBeIndex max 0, newTape)
      }
    }

    go(app.steps, app.startState, 0, Vector.empty).count(identity)
  }

  def main(args: Array[String]): Unit = {
    val input = fileLines("/day25.txt").mkString("\n")
    val parsed = appParser.parse(input).get.value

    println(run(parsed))
  }
}
