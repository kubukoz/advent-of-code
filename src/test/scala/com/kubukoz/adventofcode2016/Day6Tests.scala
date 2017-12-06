package com.kubukoz.adventofcode2016

import org.scalatest.{FlatSpec, Matchers}
import com.kubukoz.adventofcode2017.Day6._

class Day6Tests extends FlatSpec with Matchers {
  "maxBlockIndex" should "work" in {
    maxBlockIndex(List(0, 2, 7, 0)) shouldBe 2
  }

  "firstRepeatedConfig" should "work" in {
    findLoop(List(0, 2, 7, 0)) shouldBe(List(2, 4, 1, 2), 5)
  }
}
