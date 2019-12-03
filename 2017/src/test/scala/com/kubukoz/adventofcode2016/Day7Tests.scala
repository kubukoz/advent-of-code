package com.kubukoz.adventofcode2016

import com.kubukoz.adventofcode2017.Day7._
import org.scalatest.{FlatSpec, Matchers}

class Day7Tests extends FlatSpec with Matchers {
  private val input =
    """pbga (66)
      |xhth (57)
      |ebii (61)
      |havc (66)
      |ktlj (57)
      |fwft (72) -> ktlj, cntj, xhth
      |qoyq (66)
      |padx (45) -> pbga, havc, qoyq
      |tknk (41) -> ugml, padx, fwft
      |jptl (61)
      |ugml (68) -> gyxo, ebii, jptl
      |gyxo (61)
      |cntj (57)""".stripMargin.split("\n").toList
  private val tree = buildTree(input.map(parseLine)).get

  "buildTree" should "work" in {
    tree shouldBe Tree(
      "tknk",
      List(
        Tree(
          "ugml",
          List(
            Tree("gyxo", Nil, 61),
            Tree("jptl", Nil, 61),
            Tree("ebii", Nil, 61)
          ),
          68),
        Tree(
          "padx",
          List(
            Tree("havc", Nil, 66),
            Tree("qoyq", Nil, 66),
            Tree("pbga", Nil, 66)
          ),
          45
        ),
        Tree(
          "fwft",
          List(
            Tree("cntj", Nil, 57),
            Tree("ktlj", Nil, 57),
            Tree("xhth", Nil, 57)
          ),
          72
        )
      ),
      41
    )
    tree.name shouldBe "tknk"
  }

  "balanceOffset" should "be calculated properly" in {
    tree.balanceOffset.get shouldBe 60
  }
}
