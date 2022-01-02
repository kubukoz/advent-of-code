package aoc.day19

import cats.implicits._

case class ScannerDescriptor(
  id: String,
  positions: List[Position],
  movement: Position,
) {

  def relativeTo(
    pos: Position
  ): ScannerDescriptor = copy(positions = positions.map(_ |-| pos), movement = movement |+| pos)

  // minor hacky optimization: we're guaranteed to hit at least one common point even if we drop arbitrary 11 elements.
  // dropping the first 11 seems to be efficient on lists.
  def relative: LazyList[ScannerDescriptor] = positions.drop(11).to(LazyList).map(relativeTo)

  def permute: List[ScannerDescriptor] = Permutation.allCompiled.map { perm =>
    copy(positions = positions.map(perm))
  }

}
