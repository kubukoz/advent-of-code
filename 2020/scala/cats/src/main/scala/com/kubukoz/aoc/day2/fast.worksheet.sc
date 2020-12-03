import cats.effect.IO
import com.kubukoz.aoc._
import cats.effect.unsafe.implicits._
import cats.implicits._

val pat = """(\d+)\-(\d+) (\w)\: (\w+)""".r

final case class Password(from: Int, to: Int, letter: Char, value: String) {

  def isValid: Boolean =
    (from to to) contains value.count(_ == letter)

  def isValid2: Boolean = {
    val indexed = value.zipWithIndex.map(_.map(_ + 1)).map(_.swap).toMap

    List(from, to).count(indexed(_) == letter) == 1
  }

}

val passwords = Util.readFile[IO]("./files/day2.txt").unsafeRunSync().filterNot(_.trim.isEmpty()).map {
  case pat(fromDay, toDay, letter, password) => Password(fromDay.toInt, toDay.toInt, letter.head, password)
}

passwords.count(_.isValid)
passwords.count(_.isValid2)
