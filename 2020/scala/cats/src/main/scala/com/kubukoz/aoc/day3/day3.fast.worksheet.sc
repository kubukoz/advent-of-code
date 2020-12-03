import com.kubukoz.aoc._
import cats.implicits._

val baseAdvance = 3 -> 1
val allAdvances = List(1 -> 1, baseAdvance, 5 -> 1, 7 -> 1, 1 -> 2)

case class Position(x: Int, y: Int) {
  def advance(diff: (Int, Int)) = Position(x + diff._1, y + diff._2)
}

case class Line(baseFields: List[Boolean]) {
  def fieldAt(x: Int) = baseFields(x % baseFields.length)
}

val lineMap = Util.readFileUnsafe("./files/day3.txt").map(_.toList.map(_ == '#')).map(Line(_)).zipWithIndex.map(_.swap).toMap

def toField(position: Position): Option[Boolean] = lineMap.get(position.y).map(_.fieldAt(position.x))

def go(diff: (Int, Int)) = fs2.Stream.iterate(Position(0, 0))(_.advance(diff)).map(toField).unNoneTerminate.toList.count(identity).toLong

go(baseAdvance)

allAdvances.map(go).product
