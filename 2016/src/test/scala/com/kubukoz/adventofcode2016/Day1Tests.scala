package com.kubukoz.adventofcode2016

import com.kubukoz.adventofcode2016.Day1._
import org.scalatest.{FlatSpec, Matchers}

class Day1Tests extends FlatSpec with Matchers {
  "The first part" should "work for the given cases" in {
    findDistanceToLast("R2, L3") shouldBe 5
    findDistanceToLast("R2, R2, R2") shouldBe 2
    findDistanceToLast("R5, L5, R5, R3") shouldBe 12
  }

  "The second part" should "work for the given case" in {
    findDistanceToFirstRepeated("R8, R4, R4, R8") shouldBe 4
  }
}
