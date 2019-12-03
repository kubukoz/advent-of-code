package com.kubukoz.adventofcode2016

import com.kubukoz.adventofcode2016.Day2._
import org.scalatest.{FlatSpec, Matchers}

class Day2Tests extends FlatSpec with Matchers {
  "The code for the sample case" should "be valid" in {
    findCode(
      """ULL
        |RRDDD
        |LURDL
        |UUUUD""".stripMargin, keypadStr1
    ) shouldBe "1985"
  }

  it should "be valid in the new keyboard layout too" in {
    findCode(
      """ULL
        |RRDDD
        |LURDL
        |UUUUD""".stripMargin, keypadStr2
    ) shouldBe "5DB3"
  }
}
