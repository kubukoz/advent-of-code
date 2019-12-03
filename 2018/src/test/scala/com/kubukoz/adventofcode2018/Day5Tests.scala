package com.kubukoz.adventofcode2018

import cats.effect.IO
import org.scalatest.{Matchers, WordSpec}

import scala.concurrent.ExecutionContext

class Day5Tests extends WordSpec with Matchers {
  "Sample input" should {
    "work in part1" in {
      Day5.part1("dabAcCaCBAcCcaDA") shouldBe "dabCBAcaDA"

      implicit val cs = IO.contextShift(ExecutionContext.global)
      implicit val console = cats.effect.Console.io

      Day5.part2[IO]("dabAcCaCBAcCcaDA").unsafeRunSync() shouldBe Some(4)
    }
  }
}
