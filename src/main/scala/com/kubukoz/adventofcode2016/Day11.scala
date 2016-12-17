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
          !generators.contains(chip) && generators.nonEmpty
        }
    }

    def allPossibilities: List[State] = {
      for {
        Floor(currentChips, currentGenerators) <- floors.get(elevator).toList
        amountOfChips <- (0 to 2).toList
        chipsCombination <- currentChips.subsets(amountOfChips).toSet + Set.empty

        gens <- currentGenerators.subsets(2 - amountOfChips).toSet + Set.empty

        if gens.nonEmpty || chipsCombination.nonEmpty
        canGoDown = floors.filterKeys(_ < elevator).exists(_._2.nonEmpty)
        direction <- if(canGoDown) Set(-1, 1) else Set(1)
        newFloorNum = elevator + direction
        Floor(targetFloorChips, targetFloorGenerators) <- floors.get(newFloorNum)

        newCurrentFloor = Floor(currentChips -- chipsCombination, currentGenerators -- gens)
        newNextFloor = Floor(targetFloorChips ++ chipsCombination, targetFloorGenerators ++ gens)

        newFloors = floors + (newFloorNum -> newNextFloor) + (elevator -> newCurrentFloor)
      } yield State(newFloorNum, newFloors, history + 1)
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
          case (state, generatorPat(material)) => state.copy(generators = state.generators + Material.withName(material))
          case (state, microchipPat(material)) => state.copy(chips = state.chips + Material.withName(material))
          case (state, "nothing relevant") => state
        }

        intFromStr(floorNum) -> floor
    }.toMap

    State(0, floors, 0)
  }

  def main(args: Array[String]): Unit = {
    val input = fileLines("/day11-real.txt")
    val input2 = fileLines("/day11-real-2.txt")

//    println(findShortestPath(parse(input)))
    println(findShortestPath(parse(input2)))
  }
}

case class Floor(chips: Set[Material.Value], generators: Set[Material.Value]) {
  def nonEmpty: Boolean = chips.nonEmpty || generators.nonEmpty
}

object Material extends Enumeration{
  val thulium, promethium, polonium, cobalt, hydrogen, lithium, ruthenium, elerium, dilithium = Value
}
