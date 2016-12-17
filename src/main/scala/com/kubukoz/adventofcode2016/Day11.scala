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

  case class State(elevator: Int, floors: Map[Int, Floor], history: Int) {
    def blowsUp: Boolean = floors.values.exists {
      case Floor(chips, generators) =>
        chips.exists { chip =>
          !generators.exists(_.material == chip.material) && generators.nonEmpty
        }
    }

    def allPossibilities: List[State] = {
      val Floor(currentChips, currentGenerators) = floors(elevator)

      for {
        amountOfChips <- (0 to 2).toList
        chipsCombination <- currentChips.subsets(amountOfChips).toSet + Set.empty

        gens <- currentGenerators.subsets(2 - amountOfChips).toSet + Set.empty

        if gens.nonEmpty || chipsCombination.nonEmpty
        direction <- Set(-1, 1)
        newFloorNum = elevator + direction
        Floor(targetFloorChips, targetFloorGenerators) <- floors.get(newFloorNum)

        newCurrentFloor = Floor(currentChips -- chipsCombination, currentGenerators -- gens)
        newNextFloor = Floor(targetFloorChips ++ chipsCombination, targetFloorGenerators ++ gens)

        newFloors = floors + (newFloorNum -> newNextFloor) + (elevator -> newCurrentFloor)
      } yield State(newFloorNum, newFloors, history + 1)
    }.distinct

    def possibilities: List[State] = {
      allPossibilities.filterNot(_.blowsUp)
    }

    def isComplete: Boolean = {
      (floors - 3).values.forall(floor => floor.generators.isEmpty && floor.chips.isEmpty)
    }
  }

  def findShortestPath(input: State): Int = {
    @tailrec
    def goRec(possibilities: List[State]): Int = {
      possibilities.headOption.map(_.history).foreach { len =>
        println(s"current depth: $len")
      }
      val completes = possibilities.filter(_.isComplete)
      if (completes.isEmpty) goRec(possibilities.flatMap(_.possibilities).distinct)
      else completes.head.history
    }

    goRec(input.possibilities)
  }

  def parse(input: List[String]): State = {
    val floors = input.map {
      case linePat(floorNum, elems) =>
        val start = Floor(Set.empty, Set.empty)

        val floor = elems.split("""((,)? and )|(, )""").foldLeft(start) {
          case (state, generatorPat(material)) => state.copy(generators = state.generators + Generator(material))
          case (state, microchipPat(material)) => state.copy(chips = state.chips + Microchip(material))
          case (state, "nothing relevant") => state
        }

        intFromStr(floorNum) -> floor
    }.toMap

    State(0, floors, 0)
  }

  def main(args: Array[String]): Unit = {
    val input = fileLines("/day11-real.txt")
    val input2 = fileLines("/day11-real-2.txt")

    println(findShortestPath(parse(input)))
    println(findShortestPath(parse(input2)))
  }
}

case class Floor(chips: Set[Microchip], generators: Set[Generator])

case class Microchip(material: String) extends AnyVal

case class Generator(material: String) extends AnyVal
