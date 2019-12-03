package com.kubukoz.adventofcode2017

object Day1 {
  private def sumDist(dist: Int, s: Int*): Int = {
    val megaList = s ++ s

    s.zipWithIndex.map {
      case (digit, i) => List(digit, megaList(i + dist))
    }.filter(_.distinct.size == 1).map(_.head).sum
  }

  def sum1(s: Int*): Int = sumDist(1, s:_*)

  def sumHalfway(s: Int*): Int = sumDist(s.length / 2, s:_*)

  def inputSums(): (Int, Int) = {
    val input = fileLines("/day1.txt")

    val parsed: List[Int] = input.flatMap(_.map(_.toString.toInt))

    (sum1(parsed: _*), sumHalfway(parsed:_*))
  }

  def main(args: Array[String]): Unit = {
    println("Part 1, 2: " + inputSums())
  }
}
