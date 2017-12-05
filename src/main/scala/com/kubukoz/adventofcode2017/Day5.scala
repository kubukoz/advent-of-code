package com.kubukoz.adventofcode2017

object Day5 {

  def transform(ints: List[Int], delta: Int => Int): Int = {
    //Just for performance, I swear
    val intArray = ints.toArray

    val length = ints.length

    def go(index: Int, steps: Int): Int = {
      if (index > length - 1 || index < 0) steps
      else {
        val instruction = intArray(index)

        intArray(index) = delta(instruction) + instruction
        val newIndex = index + instruction

        go(newIndex, steps + 1)
      }
    }

    go(0, 0)
  }

  val part1: List[Int] => Int = transform(_, _ => 1)
  val part2: List[Int] => Int = transform(_, x => if (x >= 3) -1 else 1)

  def main(args: Array[String]): Unit = {
    val input = fileLines("/day5.txt").map(_.toInt)
    println(part1(input))
    println(part2(input))
  }
}
