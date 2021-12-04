import cats.Monad

import cats.data._
import cats.implicits._
import cats.kernel.Monoid
import cats.mtl.Raise
import cats.mtl.Stateful
import lib._

import Numeric.Implicits._
import Ordering.Implicits._

object Day4 extends App {

  def parseInput(input: String): (List[Int], List[Board[Int]]) =
    (
      input.linesIterator.toList.head.split(",").map(_.toInt).toList,
      input
        .linesWithSeparators
        .toList
        .tail
        .mkString
        .split("\n\n")
        .filterNot(_.trim.isEmpty)
        .map {
          _.split("\n")
            .filterNot(_.trim.isEmpty)
            .map(_.split("\\s+").filterNot(_.trim.isEmpty).map(_.toInt).toList)
            .toList
        }
        .toList
        .zipWithIndex
        .map((Board.apply[Int] _).tupled),
    )

  final case class Board[E: Numeric](rows: List[List[E]], index: Int) {

    def score(state: AppState[E]) =
      rows.flatten.filterNot(state.pastCalls.contains).sum * state.pastCalls.head

    def isComplete(state: AppState[E]) = List(rows, rows.transpose).exists(
      _.exists(_.forall(entry => state.pastCalls.exists(_ equiv entry)))
    )

  }

  final case class AppState[E](pastCalls: List[E])

  object AppState {
    implicit def monoid[E]: Monoid[AppState[E]] = Monoid[List[E]].imap(AppState(_))(_.pastCalls)
  }

  sealed trait EndCondition
  case object FirstCompleted extends EndCondition
  case object LastCompleted extends EndCondition

  def partF[E: Numeric, F[_]: Monad](
    calls: List[E],
    boards: List[Board[E]],
    condition: EndCondition,
  )(
    implicit S: Stateful[F, AppState[E]],
    E: Raise[F, Board[E]],
  ): F[Unit] = calls
    .traverse_ { call =>
      S.get.flatMap { state =>
        val newState = state.copy(pastCalls = call :: state.pastCalls)

        val result: F[Unit] =
          condition match {
            case FirstCompleted =>
              boards
                .find(_.isComplete(newState))
                .traverse_(E.raise)

            case LastCompleted =>
              boards
                .find(!_.isComplete(state))
                .traverse_(E.raise)
                .whenA(boards.forall(_.isComplete(newState)))
          }

        S.set(newState) *> result
      }
    }

  def part[E: Numeric](calls: List[E], boards: List[Board[E]], condition: EndCondition): E = {
    type Effect[A] = EitherT[State[AppState[E], *], Board[E], A]
    partF[E, Effect](calls, boards, condition)
      .value
      .runEmpty
      .map {
        case (state, Left(board)) => board.score(state)
        case (_, Right(_))        => throw new Exception("Impossible! There was no board found.")
      }
      .value
  }

  locally {
    val (calls, boards) = parseInput(lib.readAll("day4-example.txt"))

    assertEquals(part(calls, boards, FirstCompleted), 4512, "Part 1 example")
    assertEquals(part(calls, boards, LastCompleted), 1924, "Part 2 example")
  }

  locally {
    val (calls, boards) = parseInput(lib.readAll("day4.txt"))

    assertEquals(part(calls, boards, FirstCompleted), 82440, "Part 1")
    assertEquals(part(calls, boards, LastCompleted), 20774, "Part 2")
  }
}
