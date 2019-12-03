package com.kubukoz.adventofcode2017

import scala.Function.const

object Day15 {
  private val onesMask = (math.pow(2, 16) - 1).toInt

  case class Generator(seed: Long, factor: Int)(pred: Long => Boolean) {
    def nextNumber(currentNumber: Long): Long = {
      (currentNumber * factor) % 2147483647
    }

    val numbers: Iterator[Long] = {
      val it = Iterator.iterate(seed)(nextNumber).filter(pred)
      it.next() //eh
      it.map(_ & onesMask)
    }
  }

  def countMatching(amount: Int, g1: Generator, g2: Generator): Int = {
    (g1.numbers zip g2.numbers).take(amount)
      .count { case (a, b) => a == b }
  }

  def main(args: Array[String]): Unit = {
    val g1Seed = 883
    val g2Seed = 879
    val g1 = Generator(g1Seed, 16807) _
    val g2 = Generator(g2Seed, 48271) _


    val million = 1000000
    println(countMatching(40 * million, g1(const(true)), g2(const(true))))
    println(countMatching(5 * million,  g1(_ % 4 == 0),  g2(_ % 8 == 0)))
  }
}
