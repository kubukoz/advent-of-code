import scala.annotation.tailrec

object Day10 {
  val rounds = 55
  val input = "3"

  @tailrec
  def getNext(s: String, prefix: String = ""): String = s.length match {
    case 0 => prefix
    case l =>
      val char = s.head
      val howMany = s.takeWhile(_ == char).length
      getNext(s.substring(howMany), prefix + howMany + char)
  }

  def main(args: Array[String]) {

    (1 to rounds).foldLeft(input) { (res, i) =>
      println(s"$i round")
      getNext(res)
    }
  }
}