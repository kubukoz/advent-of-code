import com.kubukoz.aoc._
import cats.implicits._
import cats.data._

val input = Util.readFileUnsafe("./files/day9.txt").map(_.toLong: BigDecimal)

def valid[A: Numeric](line: List[A]) = {
  val last = line.last
  line.init.combinations(2).exists(_.sum == last)
}

val preambleSize = 25

val part1 = input.sliding(preambleSize + 1).filterNot(valid).map(_.last).toList.head

def part2 =
  (2 to input.size)
    .flatMap(input.sliding(_))
    .find(_.sum == part1)
    .map(r => r.min + r.max)
