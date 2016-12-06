package com.kubukoz.adventofcode2016

import scala.collection.immutable.Seq

/**
  * Created by kubukoz on 06/12/2016.
  */
object Day6 {
  private def findPasswordByFrequency(input: List[String], selectFun: List[Char] => Char): String = {
    val charsByColumn = input.map {
      _.map(List(_))
    }.reduce { (a, b) =>
      (a zip b).map { case (chars, chars2) => chars ::: chars2 }
    }

    charsByColumn.map(chList => selectFun(chList.sortBy(ch => chList.count(_ == ch)))).mkString
  }

  def findPasswordByMostPopular(input: List[String]): String = findPasswordByFrequency(input, _.last)

  def findPasswordByLeastPopular(input: List[String]): String = findPasswordByFrequency(input, _.head)

  def main(args: Array[String]): Unit = {
    val source = io.Source.fromURL(getClass.getResource("/day6.txt"))
    val input = source.getLines().toList
    source.close()

    println(findPasswordByMostPopular(input))
    println(findPasswordByLeastPopular(input))
  }
}
