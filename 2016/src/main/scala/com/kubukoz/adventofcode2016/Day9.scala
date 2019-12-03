package com.kubukoz.adventofcode2016

case class Result(start: Long, end: Long, howMany: Int, multiplier: Int)

object Day9 {
  private val markerPat = """\((\d+)x(\d+)\)""".r

  private def findPatternInString(source: String): Option[Result] = markerPat.findFirstMatchIn(source).map { matched =>
    Result(matched.start, matched.end, matched.group(1).toInt, matched.group(2).toInt)
  }

  private def decompressInternal(str: String, transformNested: String => Long): Long = findPatternInString(str).map {
    case Result(start, end, howMany, multiplier) =>
      val (toDecompress, after) = str.substring(end.toInt).splitAt(howMany)

      val decompressed = transformNested(toDecompress) * multiplier

      start + decompressed + decompressInternal(after, transformNested)
  }.getOrElse(str.length)

  val decompress: String => Long = decompressInternal(_, _.length)
  val decompressRec: String => Long = decompressInternal(_, decompressRec)

  def main(args: Array[String]): Unit = {
    val input = fileLines("/day9.txt")
    println(decompress(input.mkString))
    println(decompressRec(input.mkString))
  }
}
