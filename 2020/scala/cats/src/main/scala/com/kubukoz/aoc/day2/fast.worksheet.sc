import com.kubukoz.aoc._
import cats.implicits._

val pat = """(\d+)\-(\d+) (\w)\: (\w+)""".r

case class Password(from: Int, to: Int, letter: Char, value: String) {

  def isValid: Boolean =
    (from to to) contains value.count(_ == letter)

  def isValid2: Boolean = {
    val indexed = value.zipWithIndex.map(_.fmap(_ + 1)).map(_.swap).toMap

    List(from, to).count(indexed(_) == letter) == 1
  }

}

val passwords = Util.readFileUnsafe("./files/day2.txt").map {
  case pat(fromDay, toDay, letter, password) => Password(fromDay.toInt, toDay.toInt, letter.head, password)
}

passwords.count(_.isValid)
passwords.count(_.isValid2)
