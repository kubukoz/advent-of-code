import scala.util.Try

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

  // val input = example
  val input = fromFile

  val calls = input.linesIterator.toList.head.split(",").map(_.toInt).toList

  final case class Board(rows: List[List[Int]], index: Int) {
    def score(state: AppState) =
      rows.flatten.filterNot(state.pastCalls.contains).sum * state.pastCalls.head

    def isComplete(state: AppState) = {

      val hasRow = rows.exists(_.forall(state.pastCalls.contains))
      val hasColumn = rows.transpose.exists(_.forall(state.pastCalls.contains))

      hasRow || hasColumn
    }

  }

  val boards = input
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
    .map(Board.tupled)

  final case class AppState(pastCalls: List[Int])
  import cats.implicits._
  import cats.data._

  def part =
    calls
      .traverse { call =>
        State.get[AppState].flatMap { state =>
          val newState = state.copy(pastCalls = call :: state.pastCalls)

          // val completeBoardNow = boards.find(_.isComplete(newState))

          // if (completeBoardNow.isDefined) {
          //   println(completeBoardNow.get.score(newState))
          //   // todo
          //   ???
          // }

          val incompleteBoards = boards.filterNot(_.isComplete(newState))

          if (incompleteBoards.isEmpty) {
            println(boards.find(!_.isComplete(state)).get.score(newState))
            ???
          }

          State.set(newState)
        }

      }
      .runS(AppState(Nil))
      .value

  Try(part)
  // boards.foreach(println)
}
