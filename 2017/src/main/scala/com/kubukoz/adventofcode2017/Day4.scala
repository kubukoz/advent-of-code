package com.kubukoz.adventofcode2017

object Day4 {
  def validate(line: String, grouping: String => String): Boolean = {
    line.split(" ").groupBy(grouping).forall(_._2.tail.isEmpty)
  }

  val isValid: String => Boolean = validate(_, identity)
  val isValid2: String => Boolean = validate(_, _.sorted)

  def main(args: Array[String]): Unit = {
    val lines = fileLines("/day4.txt")
    
    println(lines.count(isValid))
    println(lines.count(isValid2))
  }
}
