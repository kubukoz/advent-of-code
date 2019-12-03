package com.kubukoz.adventofcode2016

import com.kubukoz.adventofcode2016.Day15.findFirstHole
import org.scalatest.{FlatSpec, Matchers}

class Day15Tests extends FlatSpec with Matchers {
  private val testInput = fileLines("/day15-test.txt")
  "findFirstHole" should "return 5 for the two test wheels" in {
    findFirstHole(testInput) shouldBe 5
  }

  it should "return 85 when another wheel is added" in {
    findFirstHole(testInput :+ "Disc #3 has 11 positions; at time=0, it is at position 0.") shouldBe 85
  }
}
