package com.kubukoz.adventofcode2016

case class Room(letters: String, sectorId: Int, checksum: String) {
  def isReal: Boolean = {
    val lettersByCount = letters.filterNot(_ == '-').groupBy(identity).mapValues(_.length)

    implicit val ord: Ordering[(Char, Int)] = Ordering.by { case (letter, count) => (-count, letter) }

    val calculatedChecksum = lettersByCount.toList.sorted(ord).map(_._1).take(checksum.length).mkString
    checksum == calculatedChecksum
  }

  def rotatedLetters: Room = {
    val chars = 'a' to 'z'
    val newLetters = letters.map {
      case '-' => ' '
      case ch => chars((chars.indexOf(ch) + sectorId) % chars.size)
    }

    copy(letters = newLetters)
  }
}

object Room {
  private val roomPattern = """(.+)\-(\d+)\[(.{5})\]""".r

  val fromString: String => Room = {
    case roomPattern(letters, sectorId, checksum) => Room(letters, sectorId.toInt, checksum)
  }
}

object Day4 {
  def main(args: Array[String]): Unit = {
    val src = io.Source.fromURL(getClass.getResource("/day4.txt"))
    val input = src.getLines().toList
    src.close()

    val realRooms = input.map(Room.fromString).filter(_.isReal)

    println("part 1")
    println(realRooms.map(_.sectorId).sum)
    println("part 2")
    realRooms.map(_.rotatedLetters).find(_.letters == "northpole object storage").map(_.sectorId) foreach println
  }
}
