package com.kubukoz.adventofcode2017

object Day16 {

  sealed trait DanceMove extends Product with Serializable

  case class Spin(x: Int) extends DanceMove

  case class Exchange(a: Int, b: Int) extends DanceMove

  case class Partner(a: Char, b: Char) extends DanceMove

  private def dance(string: String): DanceMove => String = {
    case Spin(x) =>
      val (left, right) = string.splitAt(string.length - x)
      right + left

    case Exchange(aIndex, bIndex) =>
      val a = string(aIndex)
      val b = string(bIndex)

      string.updated(aIndex, b).updated(bIndex, a)

    case Partner(a, b) =>
      val aIndex = string.indexOf(a)
      val bIndex = string.indexOf(b)

      string.updated(aIndex, b).updated(bIndex, a)
  }

  private val spinPat = """s(\d+)""".r
  private val exchangePat = """x(\d+)\/(\d+)""".r
  private val partnerPat = """p(\w)\/(\w)""".r

  val parse: String => DanceMove = {
    case spinPat(x) => Spin(x.toInt)
    case exchangePat(a, b) => Exchange(a.toInt, b.toInt)
    case partnerPat(a, b) => Partner(a.head, b.head)
  }

  def transform(original: String, moves: List[DanceMove], dances: Int): String = {
    def doDances(i: Int): Stream[String] = Stream.fill(i)(moves).flatten.scanLeft(original)(dance(_)(_))

    val cycleSizeOpt = doDances(dances).tail.zipWithIndex.collectFirst { case (`original`, i) => i + 1 }

    val finalDanceCount = cycleSizeOpt.fold(dances) { cycleSize =>
      val movesLeft = (moves.size * dances.toLong % cycleSize).toInt
      (movesLeft / moves.size.toDouble).ceil.toInt
    }

    doDances(finalDanceCount).last
  }

  def main(args: Array[String]): Unit = {
    val input = fileLines("/day16.txt").head.split(",").map(parse).toList
    val startPositions = ('a' to 'p').mkString

    println(transform(startPositions, input, 1))
    println(transform(startPositions, input, 1000000000))
  }
}
