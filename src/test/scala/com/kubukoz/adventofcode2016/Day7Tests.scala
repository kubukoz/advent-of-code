package com.kubukoz.adventofcode2016

import com.kubukoz.adventofcode2016.Day7._
import org.scalatest.{FlatSpec, Matchers}

class Day7Tests extends FlatSpec with Matchers {
  "supportsTLS" should "work properly for case 1" in {
    supportsTLS("abba[mnop]qrst") shouldBe true
  }

  it should "work for case 2" in {
    supportsTLS("abcd[bddb]xyyx") shouldBe false
  }

  it should "work for case 3" in {
    supportsTLS("aaaa[qwer]tyui") shouldBe false
  }

  it should "work for case 4" in {
    supportsTLS("ioxxoj[asdfgh]zxcvbn") shouldBe true
  }

  it should "work for case 5" in {
    supportsTLS("asdf[dupa]oxxo") shouldBe true
  }

  it should "work for case 6" in {
    supportsTLS("asdf[dupa]oxxo[yolo]swag") shouldBe true
  }

  it should "work for case 7" in {
    supportsTLS("asdf[ollo]oxxo[yolo]swag") shouldBe false
  }

  it should "count the input" in {
    input.count(supportsTLS) shouldBe 118
  }
/*

  "supportsSSL" should "work properly for case 1" in {
    supportsSSL("aba[bab]xyz") shouldBe true
  }

  it should "work for case 2" in {
    supportsSSL("xyx[xyx]xyx") shouldBe false
  }

  it should "work for case 3" in {
    supportsSSL("aaa[kek]eke") shouldBe true
  }

  it should "work for case 4" in {
    supportsSSL("zazbz[bzb]cdb") shouldBe true
  }

  it should "work for my case" in {
    supportsSSL("asdf[babXaba]dupa") shouldBe false
  }

  it should "count the input" in {
    input.count(supportsSSL) shouldBe 260
  }
*/

  it should "work for the weird case" in {
    supportsSSL("xtugntiubziynpzbju[onxffxfoxibzzzd]wineojjetzitpemflx[jlncrpyrujpoxluwyc]fxvfnhyqsiwndzoh[lkwwatmiesspwcqulnc]cbimtxmazbbzlvjf") shouldBe false
  }
}
