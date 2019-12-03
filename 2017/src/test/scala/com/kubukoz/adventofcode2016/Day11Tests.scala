package com.kubukoz.adventofcode2016

import com.kubukoz.adventofcode2017.Day11.Step.{NE, S, SE, SW}
import org.scalatest.{FlatSpec, Matchers}
import com.kubukoz.adventofcode2017.Day11._

class Day11Tests extends FlatSpec with Matchers {
  "distance" should "work" in {
    distance(List(NE, NE, NE)) shouldBe 3
    distance(List(NE, NE, SW, SW)) shouldBe 0
    distance(List(NE, NE, S, S)) shouldBe 2
    distance(List(SE, SW, SE, SW, SW)) shouldBe 3
  }
}
