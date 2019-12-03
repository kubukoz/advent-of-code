package com.kubukoz.adventofcode2016

import com.kubukoz.adventofcode2016.Day16._
import org.scalatest.{FlatSpec, Matchers}

class Day16Tests extends FlatSpec with Matchers {
  "transform" should "work" in {
    generate("1") shouldBe "100"
    generate("0") shouldBe "001"
    generate("11111") shouldBe "11111000000"
    generate("111100001010") shouldBe "1111000010100101011110000"
  }

  "fill" should "work for the given case" in {
    fill("10000", 20) shouldBe "10000011110010000111"
  }
}
