package com.kubukoz.adventofcode2017

object Day13 {
  private val pat = """(\d+): (\d+)""".r

  sealed trait Direction extends Product with Serializable

  case object Up extends Direction

  case object Down extends Direction

  case class Line(range: Int, value: Int, direction: Direction) {
    def nextMove: Line = {
      val newDirection = direction match {
        case Down if value == 2 => Up
        case Up if value == (range - 1) => Down
        case oldDir => oldDir
      }

      val valueDelta = direction match {
        case Down => -1
        case Up => 1
      }

      Line(range, value + valueDelta, newDirection)
    }
  }

  val parse: List[String] => Map[Int, Line] = _.map {
    case pat(depth, range) =>
      depth.toInt -> Line(range.toInt, 1, Up)
  }.toMap

  final case class Lines(lines: Map[Int, Line]) extends AnyVal {
    //mapValues not used because of stack overflow
    def nextStep: Lines = copy(lines.map { case (k, v) => k -> v.nextMove })

    def findSeverity: Int = severities.sum

    def nextStepsStream: Iterator[Lines] = Iterator.iterate(this)(_.nextStep)

    def severities: Iterator[Int] = {
      nextStepsStream.take(lines.keySet.max + 1).zipWithIndex.flatMap {
        case (state, currentIndex) =>
          state.lines.get(currentIndex).filter(_.value == 1).map(_.range * currentIndex)
      }
    }

    def findSafeDelay: Int = nextStepsStream.indexWhere(_.severities.isEmpty)
  }

  def main(args: Array[String]): Unit = {
    val input = fileLines("/day13.txt")
    val parsed = Lines(parse(input))

    println(parsed.findSeverity) //1612
    //takes about 70sec
    println(parsed.findSafeDelay) //3907994
  }
}
