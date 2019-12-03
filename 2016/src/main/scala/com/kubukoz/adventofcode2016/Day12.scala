package com.kubukoz.adventofcode2016

/**
  * Created by kubukoz on 25/12/2016.
  */
object Day12 {
  type State = Map[Char, Int]

  def transformInput(instructions: List[String], startMap: State): State = {

    val cpyNumber = """cpy (\d+) (\w)""".r
    val cpyRegister = """cpy (\w) (\w)""".r
    val incRegister = """inc (\w)""".r
    val decRegister = """dec (\w)""".r
    val jnz = """jnz (.+) (-?\d+)""".r

    val instructionCount = instructions.length

    def applyInstruction(i: Int, state: State): State = {
      if (i >= instructionCount) state
      else instructions(i) match {
        case cpyNumber(num, reg) =>
          applyInstruction(i + 1, state.updated(reg.head, num.toInt))

        case cpyRegister(from, to) =>
          applyInstruction(i + 1, state.updated(to.head, state(from.head)))

        case incRegister(reg) =>
          applyInstruction(i + 1, state.updated(reg.head, state(reg.head) + 1))

        case decRegister(reg) =>
          applyInstruction(i + 1, state.updated(reg.head, state(reg.head) - 1))

        case jnz(sth, _) if sth == "0" || state.get(sth.head).contains(0) =>
          applyInstruction(i + 1, state)

        case jnz(_, steps) =>
          applyInstruction(i + steps.toInt, state)
      }
    }

    applyInstruction(0, startMap)
  }

  def main(args: Array[String]): Unit = {
    val input = fileLines("/day12.txt")
    val zeroMap = ('a' to 'd').map(_ -> 0).toMap

    println(transformInput(input, zeroMap)('a'))
    println(transformInput(input, zeroMap + ('c' -> 1))('a'))
  }
}
