package com.kubukoz.adventofcode2016

import com.kubukoz.adventofcode2016.Day11._
import org.scalatest.{FlatSpec, Matchers}

class Day11Tests extends FlatSpec with Matchers {
  private val testInput = fileLines("/day11-test.txt")

  private val parsedStartState = State(0, FloorMap(
    Floor.fromStrings("cobalt", "elerium").value.toLong +
      (Floor.fromStrings("cobaltGenerator").value.toLong << 16) +
      (Floor.fromStrings("eleriumGenerator").value.toLong << 32)))

  private val endgame = State(3, FloorMap.from(Map(
    0 -> Floor.empty,
    1 -> Floor.empty,
    2 -> Floor.empty,
    3 -> Floor.fromStrings("cobalt", "elerium", "cobaltGenerator", "eleriumGenerator")
  )))

  "findShortestPath" should "work" in {
    findShortestPath(parsedStartState) shouldBe 11
  }

  "parse" should "work" in {
    parse(testInput) shouldBe parsedStartState
  }

  "FloorMap.values" should "work" in {
    parsedStartState.floors.get(0).get shouldBe
      Floor.fromStrings("cobalt", "elerium")
    parsedStartState.floors.get(1).get shouldBe
      Floor.fromStrings("cobaltGenerator")

    parsedStartState.floors.values shouldBe List(
      Floor.fromStrings("cobalt", "elerium"),
      Floor.fromStrings("cobaltGenerator"),
      Floor.fromStrings("eleriumGenerator"), Floor.empty)
  }

  "FloorMap.at" should "update correctly" in {
    parsedStartState.floors.at(1, Floor.fromStrings("cobalt", "elerium").value).values shouldBe List(
      Floor.fromStrings("cobalt", "elerium"),
      Floor.fromStrings("cobalt", "elerium"),
      Floor.fromStrings("eleriumGenerator"),
      Floor.empty)
  }

  "State.blowsUp" should "be false for the beginning state" in {
    parsedStartState.blowsUp shouldBe false
  }

  it should "be true for one of the allPossibilities in the beginning state" in {
    State(1, FloorMap.from(Map(
      0 -> Floor.fromStrings("cobalt"),
      1 -> Floor.fromStrings("elerium", "cobaltGenerator"),
      2 -> Floor.fromStrings("eleriumGenerator"),
      3 -> Floor.empty))).blowsUp shouldBe true
  }

  it should "be false for the second correct step" in {
    State(1, FloorMap.from(Map(
      0 -> Floor.fromStrings("elerium"),
      1 -> Floor.fromStrings("cobalt", "cobaltGenerator"),
      2 -> Floor.fromStrings("eleriumGenerator"),
      3 -> Floor.empty
    ))).blowsUp shouldBe false
  }

  it should "be false for the third correct step" in {
    State(2, FloorMap.from(Map(
      0 -> Floor.fromStrings("elerium"),
      1 -> Floor.empty,
      2 -> Floor.fromStrings("cobalt", "eleriumGenerator", "cobaltGenerator"),
      3 -> Floor.empty
    ))).blowsUp shouldBe false
  }

  it should "be false for the fourth correct step" in {
    State(1, FloorMap.from(Map(
      0 -> Floor.fromStrings("elerium"),
      1 -> Floor.fromStrings("cobalt"),
      2 -> Floor.fromStrings("eleriumGenerator", "cobaltGenerator"),
      3 -> Floor.empty
    ))).blowsUp shouldBe false
  }

  it should "be false for the fifth correct step" in {
    State(0, FloorMap.from(Map(
      0 -> Floor.fromStrings("cobalt", "elerium"),
      1 -> Floor.empty,
      2 -> Floor.fromStrings("eleriumGenerator", "cobaltGenerator"),
      3 -> Floor.empty
    ))).blowsUp shouldBe false
  }

  "isComplete" should "be false for start" in {
    parsedStartState.floors.isComplete shouldBe false
  }

  it should "be true for the finish" in {
    endgame.floors.isComplete shouldBe true
  }

  "allPossibilities" should "give 3 results in start state" in {
    parsedStartState.allPossibilities.length shouldBe 3
  }

  "possibilities" should "give 1 results in start state" in {
    parsedStartState.possibilities.length shouldBe 1
  }

  it should "give valid results for the real input" in {
    parse(fileLines("/day11-real.txt")).possibilities should contain theSameElementsAs List(
      State(1, FloorMap.from(Map(
        0 -> Floor.fromStrings("thulium", "ruthenium", "cobalt", "thuliumGenerator", "rutheniumGenerator", "cobaltGenerator"),
        1 -> Floor.fromStrings("polonium", "promethium", "promethiumGenerator", "poloniumGenerator"),
        2 -> Floor.empty,
        3 -> Floor.empty))),
      State(1, FloorMap.from(Map(
        0 -> Floor.fromStrings("thulium", "cobalt", "thuliumGenerator", "promethiumGenerator", "rutheniumGenerator", "poloniumGenerator", "cobaltGenerator"),
        1 -> Floor.fromStrings("polonium", "promethium", "ruthenium"),
        2 -> Floor.empty,
        3 -> Floor.empty))),
      State(1, FloorMap.from(Map(
        0 -> Floor.fromStrings("ruthenium", "cobalt", "thuliumGenerator", "promethiumGenerator", "rutheniumGenerator", "poloniumGenerator", "cobaltGenerator"),
        1 -> Floor.fromStrings("polonium", "promethium", "thulium"),
        2 -> Floor.empty,
        3 -> Floor.empty))),
      State(1, FloorMap.from(Map(
        0 -> Floor.fromStrings("thulium", "ruthenium", "thuliumGenerator", "promethiumGenerator", "rutheniumGenerator", "poloniumGenerator", "cobaltGenerator"),
        1 -> Floor.fromStrings("polonium", "promethium", "cobalt"),
        2 -> Floor.empty,
        3 -> Floor.empty))),
      State(1, FloorMap.from(Map(
        0 -> Floor.fromStrings("cobalt", "thuliumGenerator", "promethiumGenerator", "rutheniumGenerator", "poloniumGenerator", "cobaltGenerator"),
        1 -> Floor.fromStrings("polonium", "promethium", "thulium", "ruthenium"),
        2 -> Floor.empty,
        3 -> Floor.empty))),
      State(1, FloorMap.from(Map(
        0 -> Floor.fromStrings("ruthenium", "thuliumGenerator", "promethiumGenerator", "rutheniumGenerator", "poloniumGenerator", "cobaltGenerator"),
        1 -> Floor.fromStrings("polonium", "promethium", "thulium", "cobalt"),
        2 -> Floor.empty,
        3 -> Floor.empty))),
      State(1, FloorMap.from(Map(
        0 -> Floor.fromStrings("thulium", "thuliumGenerator", "promethiumGenerator", "rutheniumGenerator", "poloniumGenerator", "cobaltGenerator"),
        1 -> Floor.fromStrings("polonium", "promethium", "ruthenium", "cobalt"),
        2 -> Floor.empty,
        3 -> Floor.empty)))
    )
  }

  "ones" should "work" in {
    Bits.ones(0) shouldBe Integer.parseInt("1", 2)
    Bits.ones(1) shouldBe Integer.parseInt("10", 2)
    Bits.ones(2) shouldBe Integer.parseInt("100", 2)
    Bits.ones(3) shouldBe Integer.parseInt("1000", 2)
  }

  "withName" should "work" in {
    Bits.withName("thulium", 0) shouldBe 1
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
    Floor.fromStrings(Bits.materials ::: Bits.materials.map(_ + "Generator"): _*).values should contain theSameElementsAs (1 to 14)
  }
}
