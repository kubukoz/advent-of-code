package com.kubukoz.adventofcode2016

object Day6 {
  private def findPasswordByFrequency(input: List[String], selectFun: List[(Char, Int)] => (Char, Int)): String = {
    val charsByColumn = input.map {
      _.map(List(_))
    }.reduce { (a, b) =>
      (a zip b).map { case (chars, chars2) => chars ::: chars2 }
    }

    val charsWithCounts = charsByColumn.map { list => list.map(char => (char, list.count(_ == char))) }

    charsWithCounts.map(chList => selectFun(chList)._1).mkString
  }

  def findPasswordByMostPopular(input: List[String]): String = findPasswordByFrequency(input, _.maxBy(_._2))

  def findPasswordByLeastPopular(input: List[String]): String = findPasswordByFrequency(input, _.minBy(_._2))

  def main(args: Array[String]): Unit = {
    val input = fileLines("/day6.txt")

    println(findPasswordByMostPopular(input))
    println(findPasswordByLeastPopular(input))
  }
}
