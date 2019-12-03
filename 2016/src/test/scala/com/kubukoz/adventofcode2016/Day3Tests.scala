package com.kubukoz.adventofcode2016

import com.kubukoz.adventofcode2016.Day3._
import org.scalatest.{FlatSpec, Matchers}

class Day3Tests extends FlatSpec with Matchers {
  "The code for the sample case" should "be valid" in {
    countTriangles(List("""5 10 25""")) shouldBe 0
  }

  it should "count vertically too" in {
    countTrianglesVertical(
      """101 301 501
        |102 302 502
        |103 303 503
        |201 401 601
        |202 402 602
        |203 403 603""".stripMargin.split("\n").toList) shouldBe 6
  }
}
