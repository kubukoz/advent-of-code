package com.kubukoz.adventofcode2017

object Day8 {
  private val incPat = """(\w+) (\w+) (-?\d+) if (\w+) (.+) (-?\d+)""".r

  type Operator = (Int, Int) => Boolean
  val operators: String => Operator = {
    case ">" => _ > _
    case "<" => _ < _
    case ">=" => _ >= _
    case "<=" => _ <= _
    case "!=" => _ != _
    case "==" => _ == _
  }

  case class Increment(at: String, by: Int, ifAt: String, op: Operator, valueAt: Int)

  val parse: String => Increment = {
    case incPat(at, word, by, ifAt, opStr, valueAt) =>
      val byFactor = if (word == "inc") 1 else -1
      Increment(at, byFactor * by.toInt, ifAt, operators(opStr), valueAt.toInt)
  }

  def transform(increments: List[Increment]): List[Map[String, Int]] = {
    val init = Map.empty[String, Int].withDefaultValue(0)

    increments.scanLeft(init) {
      case (previous, Increment(at, by, ifAt, op, valueAt)) =>
        if (op(previous(ifAt), valueAt))
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
