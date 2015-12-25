import scala.annotation.tailrec

object Day11 {
  def main(args: Array[String]) {
    val input = "hxbxwxba"
    def substrings(s: String, state: List[String] = Nil): List[String] = s.length match {
      case z if z < 2 => state.distinct
      case _ => substrings(s.tail, s.substring(0, 2) :: state)
    }

    def atLeastTwoAndNotOverlapping(s: String, in: String) = allIndices(s, in).nonEmpty
    def allIndices(s: String, in: String, delta: Int = 0): List[Int] = in.indexOf(s) match {
      case -1 => Nil
      case i => (delta + i) :: allIndices(s, in.substring(i + 1), delta + i + 1)
    }

    def atLeastOneStraight(word: String) = word.sliding(3, 1).exists(s => s.head + 1 == s(1) && s(1) + 1 == s(2))

    val exclusions = "iol"
    def isValid(word: String) = {
      val r1 = !exclusions.exists(word contains _)
      val r2 = substrings(word).count(p => p.head == p.last) >= 2
      val r3 = atLeastOneStraight(word)
//      substrings(word).foreach{ss =>
//        println(ss, allIndices(ss, word))
//      }
      r1 && r2 && r3
    }

    @tailrec
    def nextWord(word: String, after: String = ""): String = {
      word.length match {
        case 0 => after
        case _ if word.last == 'z' => nextWord(word.init, "a" + after)
        case _ => word.init + (word.last + 1).toChar + after
      }
    }

    @tailrec
    def getValidPassword(old: String): String = nextWord(old) match {
      case newP if isValid(newP) => newP
      case _ => getValidPassword(nextWord(old))
    }

    val result = getValidPassword(input)
    println(s"first new: $result")

    println(s"second new: ${getValidPassword(result)}")
  }
}
