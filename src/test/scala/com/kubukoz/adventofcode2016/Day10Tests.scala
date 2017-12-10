package com.kubukoz.adventofcode2016

import org.scalatest.{FlatSpec, Matchers}
import com.kubukoz.adventofcode2017.Day10._

class Day10Tests extends FlatSpec with Matchers {
  "samples" should "work" in {
    part1(List(0, 1, 2, 3, 4), List(3, 4, 1, 5)) shouldBe 12
  }

  "samples of part 2" should "work" in {
    val normalState = (0 to 255).toList
    part2(normalState, "") shouldBe "a2582a3a0e66e6e86e3812dcb672a272"
    part2(normalState, "AoC 2017") shouldBe "33efeb34ea91902bb2f59c9920caa6cd"
    part2(normalState, "1,2,3") shouldBe "3efbe78a8d82f29979031a4aa0b16a9d"
    part2(normalState, "1,2,4") shouldBe "63960835bcdc130f0b66d7ff4f6a5a8e"
  }
}
