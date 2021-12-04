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

  val example = """7,4,9,5,11,17,23,2,0,14,21,24,10,16,13,6,15,25,12,22,18,20,8,19,3,26,1

22 13 17 11  0
 8  2 23  4 24
21  9 14 16  7
 6 10  3 18  5
 1 12 20 15 19

 3 15  0  2 22
 9 18 13 17  5
19  8  7 25 23
20 11 10 24  4
14 21 16 12  6

14 21 17 24  4
10 16 15  9 19
18  8 23 26 20
22 11 13  6  5
 2  0 12  3  7"""

  val fromFile = lib.readAllLines("day4.txt").mkString("\n")

  final case class Board[E: Numeric](rows: List[List[E]], index: Int) {

    def score(state: AppState[E]) =
      rows.flatten.filterNot(state.pastCalls.contains).sum * state.pastCalls.head

    def isComplete(state: AppState[E]) = List(rows, rows.transpose).exists(
      _.exists(_.forall(entry => state.pastCalls.exists(_ equiv entry)))
    )

  }

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
              val completeBoardNow = boards.find(_.isComplete(newState))

              completeBoardNow.traverse_(E.raise)

            case LastCompleted =>
              val incompleteBoards = boards.filterNot(_.isComplete(newState))

              // Defer for laziness of the .get call

              boards
                .find(!_.isComplete(state))
                .traverse_(E.raise)
                .whenA(incompleteBoards.isEmpty)
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
    val (calls, boards) = parseInput(example)

    assertEquals(part(calls, boards, FirstCompleted), 4512, "Part 1 example")
    assertEquals(part(calls, boards, LastCompleted), 1924, "Part 2 example")
  }

  locally {
    val (calls, boards) = parseInput(fromFile)
    println("Part 1: " + part(calls, boards, FirstCompleted))
    println("Part 2: " + part(calls, boards, LastCompleted))
  }
}
