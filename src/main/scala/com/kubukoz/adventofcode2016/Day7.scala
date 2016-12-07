package com.kubukoz.adventofcode2016

import scala.util.matching.Regex.Match

object Day7 {
  private val source = io.Source.fromURL(getClass.getResource("/day7.txt"))
  val input: List[String] = source.getLines().toList
  source.close()

  private def hasAbba(s: String): Boolean = {
    s.sliding(4).exists { s =>
      s(0) != s(1) && s.reverse == s
    }
  }

  private val bracketPattern = """\[(\w+)\]""".r

  def supportsTLS(s: String): Boolean = {
    hasAbba(s) && !bracketPattern.findAllMatchIn(s).exists { m => hasAbba(m.group(1)) }
  }

  private def abas(s: String, outside: List[Match]): Seq[String] = {
    val ranges = outside.map { m => m.start until m.end }

    s.indices.dropRight(2)
      .filterNot { index =>
        ranges.exists(_.contains(index))
      }
      .map(i => s.substring(i, i + 3))
      .filter { seq =>
        seq(0) != seq(1) && seq.head == seq.last
      }.toList
  }

  private def hasBab(aba: String, s: String): Boolean = {
    val bab = s"${aba(1)}${aba.head}${aba(1)}"
    s.contains(bab)
  }

  def supportsSSL(s: String): Boolean = {
    val brackets = bracketPattern.findAllMatchIn(s).toList

    abas(s, outside = brackets).exists { aba =>
      brackets.exists { m => hasBab(aba, m.group(1)) }
    }
  }

  def main(args: Array[String]): Unit = {
    println(input.count(supportsTLS))
    println(input.count(supportsSSL))
  }
}
