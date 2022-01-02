import aoc.lib._

import aoc.day19._
import cats.implicits._

case class ScannerDescriptor(id: String, positions: Set[Position]) {

  def permute: List[ScannerDescriptor] = Permutation.allCompiled.map { perm =>
    copy(positions = positions.map(perm))
  }

}

def parseScanner(text: String) =
  text match {
    case s"--- scanner $id ---$lines" =>
      ScannerDescriptor(
        id,
        lines
          .trim
          .split("\n")
          .map { case s"$x,$y,$z" => Position(x.toInt, y.toInt, z.toInt) }
          .toSet,
      )
  }

def parse(text: String) = text.split("\n\n").map(parseScanner).toList

val input = parse(readAll("day19.txt"))

val example = parseScanner("""--- scanner 0 ---
-1,-1,1
-2,-2,2
-3,-3,3
-2,-3,1
5,6,-4
8,0,7""")
