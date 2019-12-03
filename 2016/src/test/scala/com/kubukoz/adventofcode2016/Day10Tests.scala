package com.kubukoz.adventofcode2016

import org.scalatest.{FlatSpec, Matchers}
import Day10._

class Day10Tests extends FlatSpec with Matchers {
  "State transforming" should "work" in {
    val input = """value 5 goes to bot 2
                  |bot 2 gives low to bot 1 and high to bot 0
                  |value 3 goes to bot 1
                  |bot 1 gives low to output 1 and high to bot 0
                  |bot 0 gives low to output 2 and high to output 0
                  |value 2 goes to bot 2""".stripMargin.split("\n").toList

    runBots(input) shouldBe ProgramState(
      Map(
        2 -> Bot(List(2, 5)),
        1 -> Bot(List(2, 3)),
        0 -> Bot(List(3, 5))
      ),
      Map(
        0 -> 5,
        1 -> 2,
        2 -> 3
      )
    )
  }
}
