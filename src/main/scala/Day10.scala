import scala.annotation.tailrec

object Day10 {
  def main(args: Array[String]) {
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

    var result = input

    (1 to rounds).foreach { i =>
      result = getNext(result)
      println(s"${result.length} on $i round")
    }
  }
}