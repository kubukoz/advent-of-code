import org.scalatest.{FlatSpec, Matchers}
import Day1._

class Day1Tests extends FlatSpec with Matchers {
  "Part 1" should "give valid results" in {
    calculateLevel("(())") shouldBe 0
    calculateLevel("()()") shouldBe 0
    calculateLevel("(((") shouldBe 3
    calculateLevel("(()(()(") shouldBe 3
    calculateLevel("))(((((") shouldBe 3
    calculateLevel("())") shouldBe -1
    calculateLevel("))(") shouldBe -1
    calculateLevel(")))") shouldBe -3
    calculateLevel(")())())") shouldBe -3
    calculateLevel(input) shouldBe 232
  }

  "Part 2" should "give valid results too" in {
    findFirstBelow1(")") shouldBe 1
    findFirstBelow1("()())") shouldBe 5
    findFirstBelow1(input) shouldBe 1783
  }
}