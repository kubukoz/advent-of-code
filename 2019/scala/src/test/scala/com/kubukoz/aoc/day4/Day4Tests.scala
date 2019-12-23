package com.kubukoz.aoc.day4

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class Day4Tests extends AnyWordSpec with Matchers {
  "part1" should {
    "return true for 111111" in {
      Day4.part1(111111) shouldBe true
    }

    "return false for 223450" in {
      Day4.part1(223450) shouldBe false
    }

    "return false for 123789" in {
      Day4.part1(123789) shouldBe false
    }
  }

  "part2" should {
    "return true for 112233" in {
      Day4.part2(112233) shouldBe true
    }

    "return false for 123444" in {
      Day4.part2(123444) shouldBe false
    }

    "return true for 111122" in {
      Day4.part2(111122) shouldBe true
    }
  }
}
