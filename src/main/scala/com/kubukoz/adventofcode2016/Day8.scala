package com.kubukoz.adventofcode2016

object Day8 {
  type Board = List[List[Boolean]]
  val hash = '#'
  val dot = '.'

  private val startText: String = List.fill(6, 50)(dot).map(_.mkString).mkString("\n")

  private val rectPattern = """rect (\d+)x(\d+)""".r
  private val rotateColPattern = """rotate column x=(\d+) by (\d+)""".r
  private val rotateRowPattern = """rotate row y=(\d+) by (\d+)""".r

  def transformBoard(start: String, input: List[String]): String = {
    def switchRect(board: Board, a: Int, b: Int) = {
      board.take(b).map { line =>
        line.take(a).map(!_) ::: line.drop(a)
      } ::: board.drop(b)
    }

    def rotateRow(board: Board, y: Int, by: Int) = {
      val row = board(y)

      val width = row.length
      val (rowLeft, rowRight) = row.splitAt(width - by)
      board.take(y) ::: (rowRight ::: rowLeft) :: board.drop(y + 1)
    }

    val startBoard = start.split("\n").map(_.map(_ == hash).toList).toList

    val result: Board = input.foldLeft(startBoard) {
      case (board, rectPattern(a, b)) => switchRect(board, a.toInt, b.toInt)
      case (board, rotateRowPattern(y, by)) => rotateRow(board, y.toInt, by.toInt)
      case (board, rotateColPattern(x, by)) => rotateRow(board.transpose, x.toInt, by.toInt).transpose
    }

    result.map {
      _.map {
        case true => hash
        case _ => dot
      }.mkString
    }.mkString("\n")
  }

  def main(args: Array[String]): Unit = {
    val board = transformBoard(startText, fileLines("/day8.txt"))
    println(board)
    println(board.count(_ == hash))
  }
}
