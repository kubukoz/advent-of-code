package com.kubukoz.adventofcode2016

import org.scalatest.{FlatSpec, Matchers}

class Day12Tests extends FlatSpec with Matchers {
  "transformInput" should "work" in {
    Day12.transformInput(
      """cpy 41 a
        |inc a
        |inc a
        |dec a
        |jnz a 2
        |dec a""".stripMargin.split("\n").toList, Map('a' -> 0))('a') shouldBe 42
  }
}
