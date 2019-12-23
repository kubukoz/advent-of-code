package com.kubukoz.aoc.day3

import cats.implicits._
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class Day3Tests extends AnyWordSpec with Matchers {

  "part1" should {
    "work with example 1" in {
      val input =
        Day3.parse("""R75,D30,R83,U83,L12,D49,R71,U7,L72
                     |U62,R66,U55,R34,D71,R55,D58,R83""".stripMargin)

      Day3.part1(input) shouldBe 159.some
    }

    "work with example 2" in {
      val input =
        Day3.parse("""R98,U47,R26,D63,R33,U87,L62,D20,R33,U53,R51
                     |U98,R91,D20,R16,D67,R40,U7,R15,U6,R7""".stripMargin)

      Day3.part1(input) shouldBe 135.some
    }
  }

  "part2" should {
    "work with example 1" in {
      val input =
        Day3.parse("""R75,D30,R83,U83,L12,D49,R71,U7,L72
                     |U62,R66,U55,R34,D71,R55,D58,R83""".stripMargin)

      Day3.part2(input) shouldBe 610.some
    }

    "work with example 2" in {
      val input =
        Day3.parse("""R98,U47,R26,D63,R33,U87,L62,D20,R33,U53,R51
                     |U98,R91,D20,R16,D67,R40,U7,R15,U6,R7""".stripMargin)

      Day3.part2(input) shouldBe 410.some
    }
  }
}
