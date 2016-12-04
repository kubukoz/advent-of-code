package com.kubukoz.adventofcode2016

case class Room(letters: String, sectorId: Int, checksum: String) {
  def isReal: Boolean = {
    val lettersByCount = letters.filterNot(_ == '-').groupBy(identity).mapValues(_.length)
    val groupedByCount = lettersByCount.groupBy(_._2).mapValues(_.keys.toList.sorted)

    val calculatedChecksum =
      lettersByCount.values.toList.sorted.reverse
      .flatMap(groupedByCount)
      .distinct.take(5).mkString
    checksum == calculatedChecksum
  }
}

object Room {
  private val roomPattern = """(.+)\-(\d+)\[(.{5})\]""".r

  def fromString(s: String): Room = s match {
    case roomPattern(letters, sectorId, checksum) =>
      Room(letters, sectorId.toInt, checksum)
  }
}

object Day4 {
  def main(args: Array[String]): Unit = {
    val src = io.Source.fromURL(getClass.getResource("/day4.txt"))
    val input = src.getLines().toList
    src.close()

    println(input.map(Room.fromString).filter(_.isReal).map(_.sectorId).sum)
  }
}
