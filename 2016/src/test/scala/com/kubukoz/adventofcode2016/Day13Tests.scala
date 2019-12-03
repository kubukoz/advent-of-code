package com.kubukoz.adventofcode2016

import org.scalatest.{FlatSpec, Matchers}

class Day13Tests extends FlatSpec with Matchers {
  "findShortestRouteTo" should "work" in {
    Day13.findShortestRouteTo(7, 4, 10) shouldBe 11
  }
}
