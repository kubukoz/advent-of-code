package com.kubukoz.adventofcode2016

import org.scalatest.{FlatSpec, Matchers}
import com.kubukoz.adventofcode2017.Day3._

class Day3Tests extends FlatSpec with Matchers {
  "part 1" should "work" in {
    distance(1) shouldBe 0
    distance(12) shouldBe 3
    distance(23) shouldBe 2
    distance(1024) shouldBe 31
  }

  "segmentDistance" should "work" in {
    segmentDistance(0) shouldBe 0
    segmentDistance(1) shouldBe 1
    segmentDistance(2) shouldBe 1
    segmentDistance(3) shouldBe 1
    segmentDistance(4) shouldBe 1
    segmentDistance(5) shouldBe 2
    segmentDistance(6) shouldBe 2
    segmentDistance(7) shouldBe 2
    segmentDistance(8) shouldBe 2
    segmentDistance(9) shouldBe 3
  }
}
