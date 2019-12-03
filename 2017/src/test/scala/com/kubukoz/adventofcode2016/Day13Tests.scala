package com.kubukoz.adventofcode2016

import org.scalatest.{FlatSpec, Matchers}
import com.kubukoz.adventofcode2017.Day13._

class Day13Tests extends FlatSpec with Matchers {
  val input = Lines(parse(
    """0: 3
      |1: 2
      |4: 4
      |6: 4""".stripMargin.split("\n").toList))

  "findSeverity" should "work" in {
    input.findSeverity shouldBe 24
  }

  "findSafeDelay" should "work" in {
    input.findSafeDelay shouldBe 10
  }

  "newValue" should "work for ranges 2, 3 and 5" in {
    def values(range: Int, amount: Int) = (1 to amount).scanLeft(Line(range, 1, Up))((a, _) => a.nextMove).map(_.value).toList

    values(5, 15) shouldBe List(1, 2, 3, 4, 5, 4, 3, 2, 1, 2, 3, 4, 5, 4, 3, 2)
    values(2, 10) shouldBe List(1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1)
    values(3, 10) shouldBe List(1, 2, 3, 2, 1, 2, 3, 2, 1, 2, 3)
  }
}
