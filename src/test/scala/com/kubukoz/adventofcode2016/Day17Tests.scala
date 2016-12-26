package com.kubukoz.adventofcode2016

import com.kubukoz.adventofcode2016.Day17._
import org.scalatest.{FlatSpec, Matchers}

class Day17Tests extends FlatSpec with Matchers {
  "finding the shortest path" should "work for ihgpwlah" in {
    findShortestPath("ihgpwlah") shouldBe "DDRRRD"
  }

  it should "work for kglvqrro" in {
    findShortestPath("kglvqrro") shouldBe "DDUDRLRRUDRD"
  }

  it should "work for ulqzkmiv" in {
    findShortestPath("ulqzkmiv") shouldBe "DRURDRUDDLLDLUURRDULRLDUUDDDRR"
  }

  "finding the longest path" should "work for ihgpwlah" in {
    findLongestPath("ihgpwlah") shouldBe 370
  }

  it should "work for kglvqrro" in {
    findLongestPath("kglvqrro") shouldBe 492
  }

  it should "work for ulqzkmiv" in {
    findLongestPath("ulqzkmiv") shouldBe 830
  }
}
