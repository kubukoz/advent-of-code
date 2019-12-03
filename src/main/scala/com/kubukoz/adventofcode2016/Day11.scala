package com.kubukoz.adventofcode2016

import com.kubukoz.adventofcode2016.Bits.floorBits

import scala.annotation.tailrec
import scala.collection.immutable.Seq
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

  case class FloorMap(value: Long) extends AnyVal {
    def values: Seq[Floor] = (0 until 4).map(getInternal).toList

    def blowsUp: Boolean = values.exists { floor =>
      val (materials, generators) = floor.values.partition(_ <= Bits.materialCount)

      materials.exists(i => !generators.contains(i + Bits.materialCount)) && generators.nonEmpty
    }

    def isComplete: Boolean = (value & FloorMap.onesAtFirst3Floors) == 0

    private def getInternal(i: Int): Floor = Floor((FloorMap.floorOfOnes & (value >> floorBits * i)).toInt)

    def get(i: Int): Option[Floor] =
      if (i >= 0 && i <= 3) Some(getInternal(i)) else None

    def at(i: Int, newValue: Int): FloorMap = {
      val cleared = value & ~(FloorMap.floorOfOnes << i * floorBits)

      val updated = cleared | (newValue.toLong << i * floorBits)
      FloorMap(updated)
    }
  }

  object FloorMap {
    def from(floors: Map[Int, Floor]): FloorMap = {
      FloorMap(floors.map { case (i, floor) =>
        floor.value.toLong << (i * 16)
      }.sum)
    }

    val floorOfOnes: Long = java.lang.Long.parseLong("1" * floorBits, 2)
    val onesAtFirst3Floors: Long = java.lang.Long.parseLong("1" * floorBits * 3, 2)
  }

  case class State(elevator: Int, floors: FloorMap) {
    def allPossibilities: Set[State] = {
      for {
        currentFloor <- floors.get(elevator).toSet[Floor]
        direction <- Set(-1, 1)
        newFloorNum = elevator + direction
        Floor(targetFloorValue) <- floors.get(newFloorNum).toSet
        combinationSize <- Set(1, 2)
        diffCombination <- currentFloor.values.combinations(combinationSize)
        diff = diffCombination.map(i => 1 << (i - 1)).sum

        newCurrentFloor = currentFloor.value - diff
        newNextFloor = targetFloorValue + diff

        newFloors = floors.at(newFloorNum, newNextFloor).at(elevator, newCurrentFloor)
      } yield State(newFloorNum, newFloors)
    }

    def possibilities: Set[State] = {
      allPossibilities.filterNot(_.floors.blowsUp)
    }
  }

  def findShortestPath(input: State): Int = {
    @tailrec
    def goRec(possibilities: Set[State], alreadySeen: Set[State], depth: Int): Int = {

      println(s"depth: $depth")

      if (possibilities.exists(_.floors.isComplete)) depth
      else goRec(possibilities.flatMap(_.possibilities) -- alreadySeen, alreadySeen ++ possibilities, depth + 1)
    }

    goRec(input.possibilities, Set.empty, 1)
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

    State(0, FloorMap.from(floors))
  }

  def main(args: Array[String]): Unit = {
    val input = fileLines("/day11-real.txt")
    val input2 = fileLines("/day11-real-2.txt")

    println(findShortestPath(parse(input)))
    println(findShortestPath(parse(input2)))
  }
}

case class Floor(value: Int) extends AnyVal {
  def values: List[Int] = (0 until Bits.materialCount * 2).collect {
    case i if (value & (1 << i)) != 0 => i + 1
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

  val materials = List("thulium", "promethium", "polonium", "cobalt", "ruthenium", "elerium", "dilithium")

  val materialCount: Int = materials.length

  val floorBits = 16

  def withName(s: String, offset: Int): Int = 1 << materials.indexOf(s) + offset
}
