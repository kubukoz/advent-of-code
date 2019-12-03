package com.kubukoz.adventofcode2016

import com.kubukoz.adventofcode2016.Day20._
import org.scalatest.{FlatSpec, Matchers}

class Day20Tests extends FlatSpec with Matchers {
  "the sample case" should "work" in {
    val parsed = parse(
      """|5-8
         |0-2
         |4-7""".stripMargin.split("\n").toList)

    findAllOutside(parsed, max = 9) shouldBe List(3, 9)
  }
}
