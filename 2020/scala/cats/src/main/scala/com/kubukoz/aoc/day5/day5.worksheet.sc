import com.kubukoz.aoc._
import cats.implicits._

def bin(s: String): Int = Integer.valueOf(s, 2)

def parse(raw: String) =
  raw
    .map(Map('B' -> '1', 'R' -> '1').getOrElse(_, '0'))
    .splitAt(7)
    .bimap(bin, bin) match { case (row, column) => row * 8 + column }

val allIds = Util.readFileUnsafe("./files/day5.txt").map(parse)

val part1 = allIds.max
val part2 = allIds.find(n => allIds.contains(n + 2) && !allIds.contains(n + 1)).map(_ + 1)
