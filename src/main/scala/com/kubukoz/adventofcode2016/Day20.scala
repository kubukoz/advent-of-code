package com.kubukoz.adventofcode2016

import scala.annotation.tailrec
import scala.collection.immutable.NumericRange

object Day20 {
  def parse(input: List[String]): List[NumericRange[Long]] = {
    val pat = """(\d+)-(\d+)""".r
    input.map {
      case pat(from, to) => from.toLong to to.toLong by 1
    }
  }

  def findAllOutside(ranges: List[NumericRange[Long]],
                     max: Long): List[Long] = {
    /**
      * Since my input only generated 101 valid numbers,
      * storing them in a list is still a viable solution, hence `mem`
      * */
    @tailrec
    def go(i: Long, mem: List[Long]): List[Long] = {
      if (i > max) mem.reverse
      else ranges.find(_.contains(i)) match {
        case None => go(i + 1, i :: mem)
        case Some(range) =>
          //most important piece - skip whole range
          go(range.end + 1, mem)
      }
    }

    go(0, Nil)
  }

  def main(args: Array[String]): Unit = {
    val input = fileLines("/day20.txt")
    val parsed: List[NumericRange[Long]] = parse(input)

    val results = findAllOutside(parsed, max = 4294967295L)
    println("Head: " + results.head)
    println("Length: " + results.size)
  }
}
