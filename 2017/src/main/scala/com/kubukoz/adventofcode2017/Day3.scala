package com.kubukoz.adventofcode2017

object Day3 {
  val segmentLengths: Stream[Int] = Stream.iterate(1)(_ + 1).flatMap(x => Stream(x, x))

  //0-indexed segment number
  def segmentDistance(segmentNumber: Int): Int = (segmentNumber + 3) / 4

  def distance(number: Int): Int = {
    val lines: Stream[List[Int]] = segmentLengths.scanLeft(List.empty[Int]) { (previousLine, thisLength) =>
      val previousLast = previousLine.lastOption
      val diff = previousLast.getOrElse(0) + 1

      List.tabulate(thisLength)(_ + diff)
    }.tail

    lines.zipWithIndex.collectFirst { case (line, i) if line.contains(number) =>
      val fromCenter = {
        val indexInLine = line.indexOf(number)
        (line.size / 2 - indexInLine).abs
      }

      segmentDistance(i) + fromCenter
    }.get
  }


  def firstLarger(than: Int): Option[Int] = {
    type Coords = (Int, Int)

    val coords: Stream[Coords] = segmentLengths.zipWithIndex.flatMap { case (thisLength, segmentIndex) =>
      val originalX = segmentDistance(segmentIndex)
      val previousSegmentDistance = segmentDistance(segmentIndex - 1)

      val (rotX, rotY) = segmentIndex % 4 match {
        case 0 => (-1, 1)
        case 1 => (1, 1)
        case 2 => (1, -1)
        case _ => (-1, -1)
      }

      val swapped: Coords => Coords = if(segmentIndex % 2 == 0) identity else _.swap

      (0 until thisLength).toStream.map { indexInLine =>
        val originalY = indexInLine - previousSegmentDistance

        val rotated = (originalX * rotX, originalY * rotY)
        swapped(rotated)
      }
    }

    lazy val values: Stream[Int] = coords.zipWithIndex.map { case ((thisX, thisY), i) =>
      val neighborSum = coords.zipWithIndex.take(i).collect {
        case ((x, y), j) if (thisX - x).abs <= 1 && (thisY - y).abs <= 1 =>
          values(j)
      }.take(4).sum

      if (neighborSum > 0) neighborSum else 1
    }

    values.find(_ >= than)
  }

  def main(args: Array[String]): Unit = {
    println(distance(368078) == 371)

    println(firstLarger(368078).contains(369601))
  }
}
