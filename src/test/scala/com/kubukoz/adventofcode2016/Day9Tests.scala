package com.kubukoz.adventofcode2016

import com.kubukoz.adventofcode2016.Day9._
import org.scalatest.concurrent.ScalaFutures
import org.scalatest.{FlatSpec, Matchers}

class Day9Tests extends FlatSpec with Matchers with ScalaFutures {
  "Decompressing" should "work without markers" in {
    decompress("ADVENT") shouldBe "ADVENT".length
  }

  it should "work with case 1" in {
    decompress("A(1x5)BC") shouldBe "ABBBBBC".length
  }

  it should "work with case 2" in {
    decompress("(3x3)XYZ") shouldBe "XYZXYZXYZ".length
  }

  it should "work with case 3" in {
    decompress("A(2x2)BCD(2x2)EFG") shouldBe "ABCBCDEFEFG".length
  }

  it should "work with case 4" in {
    decompress("(6x1)(1x3)A") shouldBe "(1x3)A".length
  }

  it should "work with case 5" in {
    decompress("X(8x2)(3x3)ABCY") shouldBe "X(3x3)ABC(3x3)ABCY".length
  }

  "Decompressing recursively" should "work" in {
    decompressRec("(25x3)(3x3)ABC(2x3)XY(5x2)PQRSTX(18x9)(3x2)TWO(5x7)SEVEN") shouldBe 445
  }

  it should "work indeed" in {
    decompressRec("(27x12)(20x12)(13x14)(7x10)(1x12)A") shouldBe 241920
  }
}
