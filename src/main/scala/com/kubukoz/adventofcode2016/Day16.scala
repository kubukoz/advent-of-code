package com.kubukoz.adventofcode2016

import scala.annotation.tailrec

object Day16 {
  def generate(a: String): String = {
    a + 0 + a.reverse.map { case '1' => '0' case '0' => '1' }
  }

  @tailrec
  def fill(s: String, toLength: Int): String =
    if (s.length >= toLength) s.take(toLength)
    else fill(generate(s), toLength)

  @tailrec
  def checksum(s: String): String = {
    val newString = s.sliding(2, 2).map {
      case "00" | "11" => '1'
      case _ => '0'
    }.mkString

    if (newString.length % 2 == 0) checksum(newString) else newString
  }

  def transform(input: String, diskSize: Int) = checksum(fill(input, diskSize))

  def main(args: Array[String]): Unit = {

    val input = "10010000000110000"

    println(transform(input, 272))
    println(transform(input, 35651584))
  }
}
