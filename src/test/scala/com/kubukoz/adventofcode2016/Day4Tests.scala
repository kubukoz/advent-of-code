package com.kubukoz.adventofcode2016

import org.scalatest.{FlatSpec, Matchers}

class Day4Tests extends FlatSpec with Matchers {
  "Room checking" should "work for case 1" in {
    Room.fromString("aaaaa-bbb-z-y-x-123[abxyz]").isReal shouldBe true
  }
  it should "work for case 2" in {
    Room.fromString("a-b-c-d-e-f-g-h-987[abcde]").isReal shouldBe true
  }
  it should "work for case 3" in {
    Room.fromString("not-a-real-room-404[oarel]").isReal shouldBe true
  }
  it should "work for case 4" in {
    Room.fromString("totally-real-room-200[decoy]").isReal shouldBe false
  }
}
