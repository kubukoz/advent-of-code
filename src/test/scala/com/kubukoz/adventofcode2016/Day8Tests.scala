package com.kubukoz.adventofcode2016

import Day8._
import org.scalatest.{FlatSpec, Matchers}

class Day8Tests extends FlatSpec with Matchers {

  private val start =
    """.......
      |.......
      |.......""".stripMargin

  private val out1 =
    """###....
      |###....
      |.......""".stripMargin

  "The transformer" should "understand rect" in {
    transformBoard(start, "rect 3x2" :: Nil) shouldBe out1
  }

  private val out2 =
    """#.#....
      |###....
      |.#.....""".stripMargin

  it should "understand rotating the column" in {
    transformBoard(out1, "rotate column x=1 by 1" :: Nil) shouldBe out2
  }
  private val out3 =
    """....#.#
      |###....
      |.#.....""".stripMargin

  it should "understand rotating the row" in {
    transformBoard(out2, "rotate row y=0 by 4" :: Nil) shouldBe out3
  }
  private val out4 =
    """.#..#.#
      |#.#....
      |.#.....""".stripMargin

  it should "understand rotating the column again" in {
    transformBoard(out3, "rotate column x=1 by 1" :: Nil) shouldBe out4
  }
}
