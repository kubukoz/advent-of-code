import aoc.lib._

/*
#############
#...........#
###C#C#B#D###
  #D#A#B#A#
  #########


 */
val input = readAllLines("day23-example.txt")

val pods = input
  .drop(2)
  .init
  .zipWithIndex
  .map {
    case (line, 0) => line.drop(2).dropRight(2)
    case (line, _) => line
  }
  .map(_.split("#").filterNot(_.trim.isEmpty()).toList)
