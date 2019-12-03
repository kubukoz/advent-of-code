package com.kubukoz.adventofcode2017

import scala.annotation.tailrec

object Day17 {
  def transform1(initState: IndexedSeq[Int], jump: Int, maxSteps: Int): Int = {
    @tailrec
    def go(state: IndexedSeq[Int], currentIndex: Int, stepsDone: Int): IndexedSeq[Int] = {
      if(stepsDone == maxSteps) state
      else {
        val itemToInsert = stepsDone + 1

        val toSplit = (currentIndex + jump) % state.size + 1
        val (left, right) = state.splitAt(toSplit)
        val newInts = left ++ (itemToInsert +: right)

        go(newInts, left.size, itemToInsert)
      }
    }

    val result = go(initState, 0, 0)
    result(result.indexOf(2017) + 1)
  }

  def transform2(jump: Int, maxSteps: Int): Int = {
    @tailrec
    def go(state: Int, currentIndex: Int, stepsDone: Int): Int = {
      if (stepsDone == maxSteps) state
      else {
        val itemToInsert = stepsDone + 1
        val newIndex = ((currentIndex + jump) % itemToInsert) + 1
        val newValueAfterZero = if(newIndex == 1) itemToInsert else state

        go(newValueAfterZero, newIndex, itemToInsert)
      }
    }

    go(1, 1, 1)
  }

  def main(args: Array[String]): Unit = {
    val input = 369
    println(transform1(Vector(0), input, 2017)) //1547
    println(transform2(input, 50000000)) //31154878
  }
}
