package com.kubukoz.adventofcode2016

object Day3 {
  private def toMatrix(s: List[String]) = s.map(_.split("""\s+""").filterNot(_.trim.isEmpty).map(_.toInt))

  private def isTriangle(a: Int, b: Int, c: Int) = a + b > c && a + c > b && b + c > a

  def countTriangles(s: List[String]): Int = toMatrix(s).count {
    case Array(a, b, c) => isTriangle(a, b, c)
  }

  def countTrianglesVertical(s: List[String]): Int = {
    toMatrix(s).grouped(3).map {
      _.transpose.count {
        case Seq(a, b, c) => isTriangle(a, b, c)
      }
    }.sum
  }

  def main(args: Array[String]): Unit = {
    val input = fileLines("/day3.txt")

    println(countTriangles(input))
    println(countTrianglesVertical(input))
  }
}
