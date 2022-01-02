package aoc.day19

import cats.implicits._

case class ScannerDescriptor(id: String, positions: List[Position]) {

  def relativeTo(pos: Position): ScannerDescriptor = copy(positions = positions.map(_ |-| pos))

  def relative: LazyList[(Position, ScannerDescriptor)] = positions.to(LazyList).map { rel =>
    rel -> relativeTo(rel)
  }

  def permute: List[ScannerDescriptor] = Permutation.allCompiled.map { perm =>
    copy(positions = positions.map(perm))
  }

}
