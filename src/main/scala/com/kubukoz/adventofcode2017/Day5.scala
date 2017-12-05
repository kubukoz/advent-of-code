package com.kubukoz.adventofcode2017

object Day5 {

  def transform(intList: List[Int], delta: Int => Int): Int = {
    val length = intList.length

    def go(ints: Vector[Int], index: Int, steps: Int): Int = {
      if (index > length - 1 || index < 0) steps
      else {
        val instruction = ints(index)

        val newInts = ints.updated(index, delta(instruction) + instruction)
        val newIndex = index + instruction

        go(newInts, newIndex, steps + 1)
      }
    }

    go(intList.toVector, 0, 0)
  }

  val part1: List[Int] => Int = transform(_, _ => 1)
  val part2: List[Int] => Int = transform(_, x => if (x >= 3) -1 else 1)

  def main(args: Array[String]): Unit = {
    val input = fileLines("/day5.txt").map(_.toInt)
    println(part1(input))
    println(part2(input))
  }
}
