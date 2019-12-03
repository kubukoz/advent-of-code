package com.kubukoz.adventofcode2016

import scala.language.postfixOps

object Day18 {

  sealed trait TrapOrSafe extends Product with Serializable

  case object Trap extends TrapOrSafe

  case object Safe extends TrapOrSafe

  def generateTrapOrSafe: List[TrapOrSafe] => TrapOrSafe = {
    case List(Trap, Trap, Safe) => Trap
    case List(Safe, Trap, Trap) => Trap
    case List(Trap, Safe, Safe) => Trap
    case List(Safe, Safe, Trap) => Trap
    case _ => Safe
  }

  def main(args: Array[String]): Unit = {
    val input = "^.....^.^^^^^.^..^^.^.......^^..^^^..^^^^..^.^^.^.^....^^...^^.^^.^...^^.^^^^..^^.....^.^...^.^.^^.^"

    val parse: Char => TrapOrSafe = {
      case '^' => Trap
      case _ => Safe
    }

    val firstFloor = input.map(parse)

    def countSafeInRows(rowCount: Int) = {
      val (_, safes) = (1 until rowCount).foldLeft((firstFloor, firstFloor.count(_ == Safe))) {
        case ((previousFloor, sumSoFar), _) =>
          val newFloor = previousFloor.indices.map { i =>
            val dependencies = (-1 to 1).toList.map(i +)
              .map(previousFloor.lift(_).getOrElse(Safe))

            generateTrapOrSafe(dependencies)
          }

          (newFloor, sumSoFar + newFloor.count(_ == Safe))
      }

      safes
    }

    println(countSafeInRows(40))
    println(countSafeInRows(400000))
  }
}
