package com.kubukoz.adventofcode2018
import cats.effect.IO
import org.scalatest.{Matchers, WordSpec}

class Day2Tests extends WordSpec with Matchers {
  "Part 1" when {
    "sample input" should {
      "return 12" in {
        val input = List(
          "abcdef",
          "bababc",
          "abbcde",
          "abcccd",
          "aabcdd",
          "abcdee",
          "ababab"
        )

        Day2.part1[IO](fs2.Stream.emits(input)).unsafeRunSync() shouldBe 12
      }
    }
  }

  "Part 2" when {
    "sample input" should {
      "return fgij" in {
        val input =
          List("abcde", "fghij", "klmno", "pqrst", "fguij", "axcye", "wvxyz")

        Day2.part2[IO](fs2.Stream.emits(input)).unsafeRunSync() shouldBe Some("fgij")
      }
    }
  }
}
