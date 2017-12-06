package com.kubukoz.adventofcode2017

import scala.annotation.tailrec

object Day6 {
  def maxBlockIndex(blocks: List[Int]): Int = {
    val maxValue = blocks.max
    blocks.zipWithIndex.collectFirst { case (`maxValue`, i) => i }.get
  }

  def findLoop(blocks: List[Int]): List[List[Int]] = {
    val length = blocks.size

    def nextCombination(blocks: List[Int]): List[Int] = {
      val maxIndex = maxBlockIndex(blocks)
      val toDistribute = blocks(maxIndex)

      (0 until toDistribute)
        .map(x => (maxIndex + x + 1) % length)
        .foldLeft(blocks.updated(maxIndex, 0)) { (tempBlocks, nextIndex) =>
          tempBlocks.updated(nextIndex, tempBlocks(nextIndex) + 1)
        }
    }

    @tailrec
    def go(currentBlocks: List[Int], previousCombinations: List[List[Int]]): List[List[Int]] = {
      val newPrevious = currentBlocks :: previousCombinations

      if (previousCombinations.contains(currentBlocks)) newPrevious
      else go(nextCombination(currentBlocks), newPrevious)
    }

    go(blocks, Nil)
  }

  def getResults(blocks: List[Int]): (Int, Int) = {
    val results = findLoop(blocks)

    val lastResult = results.head
    (results.size - 1, results.tail.takeWhile(_ != lastResult).size + 1)
  }

  def main(args: Array[String]): Unit = {
    val numbers = fileLines("/day6.txt").flatMap(_.split("""\s+""").map(_.toInt))
    val results = getResults(numbers)

    println(s"Part 1: ${results._1}")
    println(s"Part 2: ${results._2}")
  }
}
