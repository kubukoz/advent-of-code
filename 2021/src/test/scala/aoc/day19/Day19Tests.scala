package aoc.day19

import cats.tests.CatsSuite
import cats.kernel.laws.MonoidLaws
import aoc.day19.Permutation
import cats.kernel.laws.discipline.MonoidTests
import org.scalacheck.Arbitrary
import org.scalacheck.Gen
import cats.kernel.Eq

class Day19Tests extends CatsSuite {

  implicit val arbAxisKind: Arbitrary[AxisKind] = Arbitrary(
    Gen.oneOf(AxisKind.X, AxisKind.Y, AxisKind.Z)
  )

  implicit val arbAxis: Arbitrary[Axis] = Arbitrary(Gen.resultOf(Axis.apply))

  implicit val arbPermutation: Arbitrary[Permutation] = Arbitrary {
    Gen.resultOf(Permutation.apply)
  }

  implicit val eqPermutation: Eq[Permutation] = Eq.fromUniversalEquals

  checkAll("Monoid[Permutation]", MonoidTests[Permutation].monoid)
}
