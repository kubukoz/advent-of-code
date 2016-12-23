package com.kubukoz.adventofcode2016

import com.kubukoz.adventofcode2016.Day11._
import org.scalatest.{FlatSpec, Matchers}

class Day11Tests extends FlatSpec with Matchers {
  private val testInput = fileLines("/day11-test.txt")

  private val parsedStartState = State(0, Map(
    0 -> Floor.fromStrings("cobalt", "elerium"),
    1 -> Floor.fromStrings("cobaltGenerator"),
    2 -> Floor.fromStrings("eleriumGenerator"),
    3 -> Floor.empty), 0)

  private val endgame = State(
    3, Map(
      0 -> Floor.empty,
      1 -> Floor.empty,
      2 -> Floor.empty,
      3 -> Floor.fromStrings("cobalt", "elerium", "cobaltGenerator", "eleriumGenerator")
    ), 0
  )

  "findShortestPath" should "work" in {
    findShortestPath(parsedStartState) shouldBe 11
  }

  "parse" should "work" in {
    parse(testInput) shouldBe parsedStartState
  }

  "State.blowsUp" should "be false for the beginning state" in {
    parsedStartState.blowsUp shouldBe false
  }

  it should "be true for one of the allPossibilities in the beginning state" in {
    State(1, Map(
      0 -> Floor.fromStrings("cobalt"),
      1 -> Floor.fromStrings("elerium", "cobaltGenerator"),
      2 -> Floor.fromStrings("eleriumGenerator"),
      3 -> Floor.empty), 0).blowsUp shouldBe true
  }

  it should "be false for the second correct step" in {
    State(1, Map(
      0 -> Floor.fromStrings("elerium"),
      1 -> Floor.fromStrings("cobalt", "cobaltGenerator"),
      2 -> Floor.fromStrings("eleriumGenerator"),
      3 -> Floor.empty
    ), 0).blowsUp shouldBe false
  }

  it should "be false for the third correct step" in {
    State(2, Map(
      0 -> Floor.fromStrings("elerium"),
      1 -> Floor.empty,
      2 -> Floor.fromStrings("cobalt", "eleriumGenerator", "cobaltGenerator"),
      3 -> Floor.empty
    ), 0).blowsUp shouldBe false
  }

  it should "be false for the fourth correct step" in {
    State(1, Map(
      0 -> Floor.fromStrings("elerium"),
      1 -> Floor.fromStrings("cobalt"),
      2 -> Floor.fromStrings("eleriumGenerator", "cobaltGenerator"),
      3 -> Floor.empty
    ), 0).blowsUp shouldBe false
  }

  it should "be false for the fifth correct step" in {
    State(0, Map(
      0 -> Floor.fromStrings("cobalt", "elerium"),
      1 -> Floor.empty,
      2 -> Floor.fromStrings("eleriumGenerator", "cobaltGenerator"),
      3 -> Floor.empty
    ), 0).blowsUp shouldBe false
  }

  "isComplete" should "be false for start" in {
    parsedStartState.isComplete shouldBe false
  }

  it should "be true for the finish" in {
    endgame.isComplete shouldBe true
  }

  "allPossibilities" should "give 3 results in start state" in {
    parsedStartState.allPossibilities.length shouldBe 3
  }


  "possibilities" should "give 1 results in start state" in {
    parsedStartState.possibilities.length shouldBe 1
  }

  "ones" should "work" in {
    Bits.ones(0) shouldBe Integer.parseInt("1", 2)
    Bits.ones(1) shouldBe Integer.parseInt("10", 2)
    Bits.ones(2) shouldBe Integer.parseInt("100", 2)
    Bits.ones(3) shouldBe Integer.parseInt("1000", 2)
  }

  "withName" should "work" in {
    Bits.withName("thiulum", 0) shouldBe 1
    Bits.withName("polonium", 0) shouldBe 1 << 2
    Bits.withName("polonium", 7) shouldBe 1 << 9
  }

  "values" should "split correctly" in {
    Floor(Integer.parseInt("1000100000010", 2)).values should contain theSameElementsAs List(2, 9, 13)
  }

  "fromStrings" should "work" in {
    Floor.fromStrings("cobalt", "elerium") shouldBe Floor((1 << 3) + (1 << 5))
  }

  "fromStrings" should "work with values" in {
    Floor.fromStrings(Bits.materials ::: Bits.materials.map(_ + "Generator"):_*).values should contain theSameElementsAs (1 to 14)
  }
}
