package com.kubukoz.adventofcode2016

import com.kubukoz.adventofcode2016.Day11._
import org.scalatest.{FlatSpec, Matchers}

class Day11Tests extends FlatSpec with Matchers {
  private val testInput = fileLines("/day11-test.txt")

  private val parsedStartState = State(0, Map(
    0 -> Floor(Set(Microchip("hydrogen"), Microchip("lithium")), Set()),
    1 -> Floor(Set(), Set(Generator("hydrogen"))),
    2 -> Floor(Set(), Set(Generator("lithium"))),
    3 -> Floor(Set(), Set())
  ), 0)

  private val endgame = State(
    3, Map(
      0 -> Floor(Set.empty, Set.empty),
      1 -> Floor(Set.empty, Set.empty),
      2 -> Floor(Set.empty, Set.empty),
      3 -> Floor(Set(Microchip("hydrogen"), Microchip("lithium")), Set(Generator("hydrogen"), Generator("lithium")))
    ), 0
  )

  "doMagic" should "work" in {
    findShortestPath(parsedStartState) shouldBe 11
  }

  "parse" should "work" in {
    parse(testInput) shouldBe parsedStartState
  }

  "State.blowsUp" should "be false for the beginning state" in {
    parsedStartState.blowsUp shouldBe false
  }

  it should "be false for the second correct step" in {
    State(1, Map(
      0 -> Floor(Set(Microchip("lithium")), Set()),
      1 -> Floor(Set(Microchip("hydrogen")), Set(Generator("hydrogen"))),
      2 -> Floor(Set(), Set(Generator("lithium"))),
      3 -> Floor(Set(), Set())
    ), 0).blowsUp shouldBe false
  }

  it should "be false for the third correct step" in {
    State(2, Map(
      0 -> Floor(Set(Microchip("lithium")), Set()),
      1 -> Floor(Set(), Set()),
      2 -> Floor(Set(Microchip("hydrogen")), Set(Generator("lithium"), Generator("hydrogen"))),
      3 -> Floor(Set(), Set())
    ), 0).blowsUp shouldBe false
  }

  it should "be false for the fourth correct step" in {
    State(1, Map(
      0 -> Floor(Set(Microchip("lithium")), Set()),
      1 -> Floor(Set(Microchip("hydrogen")), Set()),
      2 -> Floor(Set(), Set(Generator("lithium"), Generator("hydrogen"))),
      3 -> Floor(Set(), Set())
    ), 0).blowsUp shouldBe false
  }

  it should "be false for the fifth correct step" in {
    State(0, Map(
      0 -> Floor(Set(Microchip("hydrogen"), Microchip("lithium")), Set()),
      1 -> Floor(Set(), Set()),
      2 -> Floor(Set(), Set(Generator("lithium"), Generator("hydrogen"))),
      3 -> Floor(Set(), Set())
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
}
