package aoc.day19

import aoc.day19.Permutation
import cats.kernel.Eq
import cats.kernel.laws.discipline.MonoidTests
import cats.tests.CatsSuite
import org.scalacheck.Arbitrary
import org.scalacheck.Gen
import org.scalatest.matchers.should.Matchers
import cats.kernel.laws.discipline.CommutativeGroupTests

class Day19Tests extends CatsSuite with Matchers {

  implicit val arbAxisKind: Arbitrary[AxisKind] = Arbitrary(
    Gen.oneOf(AxisKind.X, AxisKind.Y, AxisKind.Z)
  )

  implicit val arbAxis: Arbitrary[Axis] = Arbitrary(Gen.resultOf(Axis.apply))

  implicit val arbPermutation: Arbitrary[Permutation] = Arbitrary {
    Gen.resultOf(Permutation.apply)
  }

  implicit val eqPermutation: Eq[Permutation] = Eq.fromUniversalEquals

  implicit val arbPosition: Arbitrary[Position] = Arbitrary(Gen.resultOf(Position.apply))
  implicit val eqPosition: Eq[Position] = Eq.fromUniversalEquals

  checkAll("Monoid[Permutation]", MonoidTests[Permutation].monoid)

  checkAll("CommutativeGroup[Position]", CommutativeGroupTests[Position].commutativeGroup)
  test("Permutation.id is identity") {
    forAll { pos: Position =>
      Permutation.id.compile(pos) shouldBe pos
    }
  }
}
