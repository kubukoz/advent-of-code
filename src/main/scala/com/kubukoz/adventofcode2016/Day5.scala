package com.kubukoz.adventofcode2016

import scala.annotation.tailrec

object Day5 {
  private def md5(s: String) =
    java.security.MessageDigest.getInstance("MD5").digest(s.getBytes).take(4).map("%02X".format(_)).mkString

  private def findPassword(s: String, rounds: Int, fun: (Int, String) => (Int, Char)): String = {
    @tailrec
    def go(mem: Map[Int, Char], lastNum: Int): Map[Int, Char] = {
      @tailrec
      def findNextNumber(current: Int): (Int, String) = {
        val result = md5(s + current)
        if (result.startsWith("0" * 5) && result(5).asDigit < rounds) (current, result)
        else findNextNumber(current + 1)
      }

      val (nextNum, nextNumResult) = findNextNumber(lastNum)
      val funResult = fun(nextNum, nextNumResult)

      val newMem =
        if (mem.isDefinedAt(funResult._1)) mem
        else mem + funResult

      if (newMem.size == rounds) newMem
      else go(newMem, nextNum + 1)
    }

    val theMap = go(Map.empty, 0)

    (0 until rounds).map(theMap).mkString
  }

  def findEasyPassword(s: String): String = findPassword(s, 8, (i, s) => (i, s(5)))

  def findHardPassword(s: String): String = findPassword(s, 8, (_, s) => (s(5).asDigit, s(6)))

  def main(args: Array[String]): Unit = {
    val input = "ffykfhsq"
    println(findEasyPassword(input))
    println(findHardPassword(input))
  }
}
