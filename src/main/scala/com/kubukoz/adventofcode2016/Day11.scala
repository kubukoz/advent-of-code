package com.kubukoz.adventofcode2016

import scala.annotation.tailrec
import scala.language.{postfixOps, reflectiveCalls}

object Day11 {
  private val linePat = """The (\w+) floor contains (.+)\.""".r
  private val generatorPat = """an? (\w+) generator""".r
  private val microchipPat = """an? (\w+)\-compatible microchip""".r

  val intFromStr: String => Int = {
    case "first" => 0
    case "second" => 1
    case "third" => 2
    case "fourth" => 3
  }

  case class State(elevator: Int, floors: Map[Int, Floor]) {
    def blowsUp: Boolean = floors.values.exists { floor =>
      val values = floor.values
      values.filter(_ <= Bits.materialCount).exists(i => !values.contains(i + Bits.materialCount)) && values.exists(_ > Bits.materialCount)
    }

    def allPossibilities: List[State] = {
      for {
        currentFloor <- floors.get(elevator).toList
        direction <- Set(-1, 1)
        newFloorNum = elevator + direction
        Floor(targetFloorValue) <- floors.get(newFloorNum).toList
        combinationSize <- Set(1, 2)
        diffCombination <- currentFloor.values.combinations(combinationSize).toList
        diff = diffCombination.map(i => 1 << (i - 1)).sum

        newCurrentFloor = Floor(currentFloor.value - diff)
        newNextFloor = Floor(targetFloorValue + diff)

        newFloors = floors + (newFloorNum -> newNextFloor) + (elevator -> newCurrentFloor)
      } yield State(newFloorNum, newFloors)
    }

    def possibilities: List[State] = {
      allPossibilities.filterNot(_.blowsUp)
    }

    def isComplete: Boolean = {
      !(floors - 3).values.exists(_.nonEmpty)
    }
  }

  def findShortestPath(input: State): Int = {
    @tailrec
    def goRec(possibilities: List[State], depth: Int): Int = {
      println(s"current depth: $depth, possibilities: ${possibilities.length}")

      possibilities.filter(_.isComplete) match {
        case Nil => goRec(possibilities.flatMap(_.possibilities).distinct, depth + 1)
        case _ => depth
      }
    }

    goRec(input.possibilities, 1)
  }

  def parse(input: List[String]): State = {
    val floors = input.map {
      case linePat(floorNum, elems) =>
        val start = Floor(0)

        val floor = elems.split("""((,)? and )|(, )""").foldLeft(start) {
          case (Floor(value), generatorPat(material)) => Floor(value + Bits.withName(material, Bits.materialCount))
          case (Floor(value), microchipPat(material)) => Floor(value + Bits.withName(material, 0))
          case (state, "nothing relevant") => state
        }

        intFromStr(floorNum) -> floor
    }.toMap

    State(0, floors)
  }

  def main(args: Array[String]): Unit = {
    val input = fileLines("/day11-real.txt")
    val input2 = fileLines("/day11-real-2.txt")

    println(findShortestPath(parse(input)))
    //    println(findShortestPath(parse(input2)))
  }
}

case class Floor(value: Int) extends AnyVal {
  def values: List[Int] = (0 until Bits.materialCount + 7).collect {
    case i if (value & Bits.ones(i)) != 0 => i + 1
  }.toList

  def nonEmpty: Boolean = value != 0
}

object Floor {
  private val generatorPat = """(\w+)Generator""".r

  def fromStrings(strings: String*): Floor = Floor(strings.map {
    case generatorPat(name) => Bits.withName(name, Bits.materialCount)
    case name => Bits.withName(name, 0)
  }.sum)

  val empty = Floor(0)
}

object Bits {

  val materials = List("thiulum", "promethium", "polonium", "cobalt", "ruthenium", "elerium", "dilithium")

  val materialCount: Int = materials.length

  def withName(s: String, offset: Int): Int = 1 << materials.indexOf(s) + offset

  def ones(offset: Int): Int = 1 << offset
}
