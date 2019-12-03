package com.kubukoz.adventofcode2017

object Day10 {

  case class State(values: List[Int], position: Int, skip: Int)

  def part1(values: List[Int], ranges: List[Int]): Int = {
    transformInternal(State(values, 0, 0), ranges).values.take(2).product
  }

  def part2(initialValues: List[Int], inputStr: String): String = {
    val input = inputStr.map(_.toInt).toList

    val inputWithAppendix = input ::: List(17, 31, 73, 47, 23)
    val sparseHash = (1 to 64).foldLeft(State(initialValues, 0, 0))((state, _) => transformInternal(state, inputWithAppendix))

    val denseHash = sparseHash.values.grouped(16).map(_.reduce(_ ^ _)).toList

    denseHash.map(_.formatted("%02x")).mkString
  }

  private def transformInternal(state: State, allRanges: List[Int]): State = allRanges.foldLeft(state) { (state, range) =>
    val length = state.values.size

    val values = state.values
    val position = state.position
    val skip = state.skip

    val splitPoint = position % length
    val (before, after) = values.splitAt(splitPoint)
    val newStr = after ::: before

    val (toReverse, toAppend) = newStr.splitAt(range)

    val (newBefore, newAfter) = (toReverse.reverse ::: toAppend).splitAt(length - splitPoint)

    val newValues = newAfter ::: newBefore

    State(newValues, position + skip + range, skip + 1)
  }

  def main(args: Array[String]): Unit = {
    val input = "129,154,49,198,200,133,97,254,41,6,2,1,255,0,191,108"

    val initialState = (0 to 255).toList

    val part1Input = input.split(",").map(_.toInt).toList

    println(part1(initialState, part1Input))
    println(part2(initialState, input))
  }
}
