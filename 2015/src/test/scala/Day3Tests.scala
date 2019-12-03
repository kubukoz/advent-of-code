import Day3._
import org.scalatest.{FlatSpec, Matchers}

class Day3Tests extends FlatSpec with Matchers {
  "Part 1" should "be valid" in {
    visitedByOne("^v^v^v^v^v") shouldBe 2
  }
  "Part 2" should "be valid" in {
    visitedByTwo("^v^v^v^v^v") shouldBe 11
  }
}
