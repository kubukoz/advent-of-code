package com.kubukoz.adventofcode2017

import enumeratum._

import scala.annotation.tailrec
import scala.collection.immutable

object Day11 {

  sealed trait Step extends EnumEntry {
    lazy val reverse: Step = Step.applySafe(Step.indexOf(this) + Step.values.size / 2)
    lazy val next: Step = Step.applySafe(Step.indexOf(this) + 1)
  }

  case class ReduceRule(combination: List[Step], result: List[Step])

  object Step extends Enum[Step] {
    case object N extends Step
    case object NE extends Step
    case object SE extends Step
    case object S extends Step
    case object SW extends Step
    case object NW extends Step

    override def values: immutable.IndexedSeq[Step] = findValues
    def applySafe(i: Int): Step = values(i % values.size)

    val rules: Set[ReduceRule] = {
      val removeReverse = values.map(v => ReduceRule(List(v, v.reverse), Nil))

      val reduceToAverage = values.map { v =>
        ReduceRule(List(v, v.next.next), List(v.next))
      }

      (reduceToAverage ++ removeReverse).toSet
    }
  }

  @tailrec
  private def reduced(steps: List[Step]): List[Step] = {
    val ruleToUse = Step.rules.find(_.combination.forall(steps.contains))

    ruleToUse match {
      case Some(ReduceRule(combination, result)) => reduced(steps.diff(combination) ::: result)
      case _ => steps
    }
  }

  def distance(steps: List[Step]): Int = reduced(steps).size

  def distances(steps: List[Step]): List[Int] = {
    steps.scanLeft(List.empty[Step]) { (previousSteps, step) =>
      reduced(step :: previousSteps)
    }.map(_.size)
  }

  def main(args: Array[String]): Unit = {
    val input = fileLines("/day11.txt").mkString("\n").split(",").map(Step.withNameInsensitive).toList
    println(distance(input))
    println(distances(input).max)
  }
}
