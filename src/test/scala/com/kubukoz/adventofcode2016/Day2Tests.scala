package com.kubukoz.adventofcode2016

import org.scalatest.{FlatSpec, Matchers}
import cats.instances.int._

import com.kubukoz.adventofcode2017.Day2._
class Day2Tests extends FlatSpec with Matchers {
  "The first part" should "work for the given cases" in {
    val input =
      """5 1 9 5
        |7 5 3
        |2 4 6 8""".stripMargin.split("\n").map(parseLine).toList

    part1(input) shouldBe 18
  }

  "The second part" should "work for the given cases" in {
    val input =
      """5 9 2 8
        |9 4 7 3
        |3 8 6 5""".stripMargin.split("\n").map(parseLine).toList

    part2(input) shouldBe 9
  }
}
