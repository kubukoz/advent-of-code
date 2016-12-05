package com.kubukoz.adventofcode2016

object Day5 {
  private val md = java.security.MessageDigest.getInstance("MD5")
  private val rounds = 8

  private def md5(s: String) = "%1$032X".format(BigInt(1, md.digest(s.getBytes)))

  private def findPassword(s: String, validateMd5: String => Boolean, fun: (Int, String) => (Int, Char)): String = {
    @annotation.tailrec
    def go(mem: Map[Int, Char], lastNum: Int): Map[Int, Char] = {
      @annotation.tailrec
      def findNextNumber(current: Int): (Int, String) = {
        val result = md5(s + current)
        if (result.startsWith("00000") && validateMd5(result)) (current, result)
        else findNextNumber(current + 1)
      }

      val (nextNum, nextNumMD5) = findNextNumber(lastNum)
      val (index, char) = fun(mem.size, nextNumMD5)

      val newMem = if (mem.isDefinedAt(index)) mem else mem + (index -> char)

      if (newMem.size == rounds) newMem
      else go(newMem, nextNum + 1)
    }

    val theMap = go(Map.empty, 0)
    (0 until rounds).map(theMap).mkString
  }

  def findEasyPassword(s: String): String = findPassword(s, _ => true, (i, s) => (i, s(5)))

  def findHardPassword(s: String): String =
    findPassword(s, _.charAt(5).asDigit < rounds, (_, s) => (s(5).asDigit, s(6)))

  def main(args: Array[String]): Unit = {
    val input = "ffykfhsq"
    println(findEasyPassword(input))
    println(findHardPassword(input))
  }
}
