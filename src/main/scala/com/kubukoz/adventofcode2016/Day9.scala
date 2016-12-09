package com.kubukoz.adventofcode2016

case class Result(start: Long, end: Long, howMany: Int, multiplier: Int)

object Day9 {
  private val markerPat = """\((\d+)x(\d+)\)""".r

  private def findPatternInString(source: String): Option[Result] = markerPat.findFirstMatchIn(source) map { matched =>
    Result(matched.start, matched.end, matched.group(1).toInt, matched.group(2).toInt)
  }

  private def decompressInternal(str: String, recursive: Boolean): Long = findPatternInString(str).map {
    case Result(start, end, howMany, multiplier) =>

      val toDecompress = str.substring(end.toInt, end.toInt + howMany)

      val decompressed = if (recursive)
        decompressRec(toDecompress)
      else
        toDecompress.length

      val after = str.drop(end.toInt + howMany)

      start + decompressed * multiplier + decompressInternal(after, recursive)
  }.getOrElse(str.length)

  val decompress: String => Long = decompressInternal(_, recursive = false)
  val decompressRec: String => Long = decompressInternal(_, recursive = true)

  def main(args: Array[String]): Unit = {
    val input = fileLines("/day9.txt")
    println(decompress(input.mkString))
    println(decompressRec(input.mkString))
  }
}
