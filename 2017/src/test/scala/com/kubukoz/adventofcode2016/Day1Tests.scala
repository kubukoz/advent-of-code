package com.kubukoz.adventofcode2017

import com.kubukoz.adventofcode2017.Day1._
import org.scalatest.{FlatSpec, Matchers}

class Day1Tests extends FlatSpec with Matchers {
  "The first part" should "work for the given cases" in {
    sum1(1, 1, 2, 2) shouldBe 3
    sum1(1, 1, 1, 1) shouldBe 4
    sum1(1, 2, 3, 4) shouldBe 0
    sum1(9, 1, 2, 1, 2, 1, 2, 9) shouldBe 9
    sum1(4, 9, 4, 7, 5, 1, 1, 3, 6, 8, 4) shouldBe 5
  }

  "The second part" should "work for the given cases" in {
    sumHalfway(1, 2, 1, 2) shouldBe 6
    sumHalfway(1, 2, 2, 1) shouldBe 0
    sumHalfway(1, 2, 3, 4, 2, 5) shouldBe 4
    sumHalfway(1, 2, 3, 1, 2, 3) shouldBe 12
    sumHalfway(1, 2, 1, 3, 1, 4, 1, 5) shouldBe 4
  }

  "Inputs" should "be properly handled" in {
    inputSums() shouldBe (1150, 1064)
  }
}
