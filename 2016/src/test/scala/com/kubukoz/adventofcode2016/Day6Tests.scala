package com.kubukoz.adventofcode2016

import org.scalatest.{FlatSpec, Matchers}
import Day6._

class Day6Tests extends FlatSpec with Matchers {
  private val testInput =
    """eedadn
      |drvtee
      |eandsr
      |raavrd
      |atevrs
      |tsrnev
      |sdttsa
      |rasrtv
      |nssdts
      |ntnada
      |svetve
      |tesnvt
      |vntsnd
      |vrdear
      |dvrsen
      |enarar""".stripMargin.split("\n").toList

  "Decoding" should "work" in {
    findPasswordByMostPopular(testInput) shouldBe "easter"
  }

  it should "work in reverse too" in {
    findPasswordByLeastPopular(testInput) shouldBe "advent"
  }
}
