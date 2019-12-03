package com.kubukoz.adventofcode2016

object Day7 {
  val input: List[String] = fileLines("/day7.txt")

  private val bracketPattern = """\[(\w+)\]""".r

  def supportsTLS(s: String): Boolean = {
    hasAbba(s) && !bracketPattern.findAllMatchIn(s).map(_.group(1)).exists(hasAbba)
  }

  private def hasAbba(s: String): Boolean = s.sliding(4).exists(isAbbaOrAba)

  def supportsSSL(s: String): Boolean = {
    val brackets = bracketPattern.findAllMatchIn(s).toList

    s.split(bracketPattern.regex).flatMap(abas).exists { aba =>
      brackets.exists { m => hasBab(aba, m.group(1)) }
    }
  }

  private def abas(s: String): Seq[String] = s.sliding(3).filter(isAbbaOrAba).toList

  private def hasBab(aba: String, s: String): Boolean = s.contains(aba(1) + aba.take(2))

  private def isAbbaOrAba(s: String) = s(0) != s(1) && s.reverse == s

  def main(args: Array[String]): Unit = {
    println(input.count(supportsTLS))
    println(input.count(supportsSSL))
  }
}
