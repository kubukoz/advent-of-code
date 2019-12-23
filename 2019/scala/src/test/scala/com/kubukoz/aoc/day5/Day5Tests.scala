package com.kubukoz.aoc.day5

import com.kubukoz.aoc.day5.data.{CommandHeader, Mode, Token}
import org.scalatest.OptionValues
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class Day5Tests extends AnyWordSpec with Matchers with OptionValues {

  private val parseCommand: String => CommandHeader = {
    CommandHeader.parser
  }
  "instruction parser" should {

    "decode if some modes are present" in {
      parseCommand("1102") shouldBe CommandHeader(
        List(
          Mode.Immediate,
          Mode.Immediate,
          Mode.Positional
        ),
        Token.Mult
      )
    }

    "decode if all modes are present (last mode positional)" in {
      parseCommand("01102") shouldBe CommandHeader(
        List(
          Mode.Immediate,
          Mode.Immediate,
          Mode.Positional
        ),
        Token.Mult
      )
    }

    "decode if all modes are present" in {
      parseCommand("10102") shouldBe CommandHeader(
        List(
          Mode.Immediate,
          Mode.Positional,
          Mode.Immediate
        ),
        Token.Mult
      )
    }

    "decode if no modes are present" in {
      val expected = CommandHeader(
        List(
          Mode.Positional,
          Mode.Positional,
          Mode.Positional
        ),
        Token.Mult
      )

      parseCommand("2") shouldBe expected
      parseCommand("02") shouldBe expected
    }
  }

}
