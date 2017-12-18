package com.kubukoz.adventofcode2017

import scala.annotation.tailrec

object Day14 {
  private val knotHash: String => String = com.kubukoz.adventofcode2017.Day10.part2((0 to 255).toList, _)

  private type AddrSet = Set[(Int, Int)]

  def getOnes(input: String): AddrSet = {
    val zeroes = "0" * 4

    val hashes = (0 until 128).map(i => s"$input-$i").map(knotHash)

    val matrix = hashes.map(
      _.toList.flatMap { char =>
        val str = Integer.parseInt(char.toString, 16).toBinaryString
        (zeroes + str).takeRight(4)
      }
    )

    for {
      (row, y) <- matrix.zipWithIndex
      ('1', x) <- row.zipWithIndex
    } yield (x, y)
  }.toSet

  private val inRange: ((Int, Int), (Int, Int)) => Boolean = {
    case ((x1, y1), (x2, y2)) =>
      ((x1 - x2).abs, (y1 - y2).abs) match {
        case (0, 1) => true
        case (1, 0) => true
        case _ => false
      }
  }

  private def removeRegionContaining(bitMap: AddrSet, x: Int, y: Int): AddrSet = {
    @tailrec
    def go(remaining: AddrSet, currentRegion: AddrSet): AddrSet = {
      remaining.find(point => currentRegion.exists(inRange(_, point))) match {
        case Some(point) => go(remaining - point, currentRegion + point)
        case None => remaining
      }
    }

    go(bitMap - ((x, y)), Set((x, y)))
  }

  def countRegions(bitMap: AddrSet): Int = {
    @tailrec
    def go(bitMap: AddrSet, regionCount: Int): Int = {
      bitMap.headOption match {
        case None => regionCount
        case Some((x, y)) =>
          val remaining = removeRegionContaining(bitMap, x, y)

          println(s"found region ${regionCount + 1}")
          go(remaining, regionCount + 1)
      }
    }

    go(bitMap, 0)
  }

  def main(args: Array[String]): Unit = {
    val input = "ffayrhll"

    val bitMap = getOnes(input)

    println(bitMap.size)
    println(countRegions(bitMap))
  }
}
