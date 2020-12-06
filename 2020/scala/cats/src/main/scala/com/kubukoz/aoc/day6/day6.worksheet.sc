import com.kubukoz.aoc._
import cats.implicits._

val input = Util.readFileUnsafe(
  "./files/day6.txt"
)

val groups = input.mkString("\n").split("\n\n").map(_.split("\n").toList).toList

val questions = 'a' to 'z'

def matches(group: List[String]): Int = questions.count(q => group.exists(_.contains(q)))
def matches2(group: List[String]): Int = questions.count(q => group.forall(_.contains(q)))

groups.foldMap(matches)
groups.foldMap(matches2)
