package com.kubukoz

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import com.kubukoz.aoc.day2.Day2
import com.kubukoz.aoc.day2.Interpreter
import cats.effect.SyncIO

class Day2Tests extends AnyWordSpec with Matchers {
  "part1" when {
    genTests(
      "1,9,10,3,2,3,11,0,99,30,40,50" -> 3500,
      "1,0,0,0,99"                    -> 2,
      "2,3,0,3,99"                    -> 2,
      "2,4,4,5,99,0"                  -> 2,
      "1,1,1,4,99,5,6,0,99"           -> 30
    )
  }

  def genTests(examples: (String, Int)*): Unit = examples.foreach {
    case (input, out) =>
      input should {
        "be " + out in {
          Interpreter
            .fromInput[SyncIO](Day2.parse(input))
            .flatMap(_.runProgram)
            .map(_ shouldBe out)
            .unsafeRunSync()
        }
      }

  }
}
