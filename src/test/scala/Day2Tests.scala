import Day2._
import org.scalatest.{FlatSpec, Matchers}

class Day2Tests extends FlatSpec with Matchers {
  "Wrapping paper" should "be calculated well" in {
    wrappingPaper(Array("2x3x4")) shouldBe 58
  }

  "Ribbon" should "be calculated well too" in {
    ribbon(Array("2x3x4")) shouldBe 34
  }
}