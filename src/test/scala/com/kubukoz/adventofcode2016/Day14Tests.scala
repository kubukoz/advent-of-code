package com.kubukoz.adventofcode2016

import org.scalatest.{FlatSpec, Matchers}
import Day14._

class Day14Tests extends FlatSpec with Matchers {
  "tripleProducer" should "work" in {
    keys("abc", 1).take(2).toList shouldBe List(39, 92)
  }

  it should "find the 64th key" in {
    keys("abc", 1)(63) shouldBe 22728
  }

  "md5 times 2017" should "work" in {
    md5Times("abc0", 2017) shouldBe "a107ff634856bb300138cac6568c0f24"
  }
}
