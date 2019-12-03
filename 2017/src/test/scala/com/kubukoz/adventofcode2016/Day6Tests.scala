package com.kubukoz.adventofcode2016

import org.scalatest.{FlatSpec, Matchers}
import com.kubukoz.adventofcode2017.Day6._

class Day6Tests extends FlatSpec with Matchers {
  private val input = List(0, 2, 7, 0)

  "maxBlockIndex" should "work" in {
    maxBlockIndex(input) shouldBe 2
  }

  "the solution" should "work" in {
    val list = findLoop(input)
    list.head shouldBe List(2, 4, 1, 2)
    getResults(input) shouldBe (5, 4)
  }
}
