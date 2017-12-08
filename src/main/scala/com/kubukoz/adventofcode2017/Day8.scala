package com.kubukoz.adventofcode2017
import enumeratum._

import scala.collection.immutable.IndexedSeq

object Day8 {
  private val incPat = """(\w+) (\w+) (-?\d+) if (\w+) (.+) (-?\d+)""".r

  sealed class Operator(val str: String, val op: (Int, Int) => Boolean) extends EnumEntry
  object Operator extends Enum[Operator] {
    case object Gt extends Operator(">", _ > _)
    case object Lt extends Operator("<", _ < _ )
    case object Gte extends Operator(">=", _ >= _)
    case object Lte extends Operator("<=", _ <= _)
    case object Ne extends Operator("!=", _ != _)
    case object Eq extends Operator("==", _ == _)

    override def values: IndexedSeq[Operator] = findValues
    val byOp: String => Operator = s => values.find(_.str == s).get
  }


  case class Increment(at: String, by: Int, ifAt: String, op: Operator, valueAt: Int)

  private val parseOp: String => Operator = Operator.byOp

  val parse: String => Increment = {
    case incPat(at, word, by, ifAt, opStr, valueAt) =>
      val byFactor = if(word == "inc") 1 else -1
      Increment(at, byFactor * by.toInt, ifAt, parseOp(opStr), valueAt.toInt)
  }

  def transform(increments: List[Increment]): List[Map[String, Int]] = {
    val init = Map.empty[String, Int].withDefaultValue(0)

    increments.scanLeft(init){
      case (previous, Increment(at, by, ifAt, op, valueAt)) =>
        if(op.op(previous(ifAt), valueAt))
          previous.updated(at, previous(at) + by)
        else previous
    }
  }

  def main(args: Array[String]): Unit = {
    val parsed = fileLines("/day8.txt").map(parse)

    val transformed = transform(parsed)
    println(transformed.last.values.max)
    println(transformed.flatMap(_.values).max)
  }
}
