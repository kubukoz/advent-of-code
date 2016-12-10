package com.kubukoz.adventofcode2016

import scala.annotation.tailrec

object Day10 {
  private val valueGoesPat = """value (\d+) goes to bot (\d+)""".r
  private val botGivesPat = """bot (\d+) gives low to (\w+) (\d+) and high to (\w+) (\d+)""".r

  @tailrec
  def getFinalState(instructions: List[Instruction], mem: ProgramState): ProgramState = instructions match {
    case Nil => mem
    case _ =>
      val (available, unavailable) = instructions.partition(mem.isAvailable)
      getFinalState(unavailable, available.foldLeft(mem)(_ updated _))
  }

  def runBots(input: List[String]): ProgramState = {
    val instructions = input.map {
      case valueGoesPat(value, botId) => ValueInput(value.toInt, botId.toInt)
      case botGivesPat(botId, lowTarget, lowTargetId, highTarget, highTargetId) =>
        Give(botId.toInt, Receiver(lowTarget, lowTargetId.toInt), Receiver(highTarget, highTargetId.toInt))
    }

    getFinalState(instructions, ProgramState(Map.empty, Map.empty))
  }

  def main(args: Array[String]): Unit = {
    val input = fileLines("/day10.txt")

    val finalState = runBots(input)

    val soughtBot = finalState.bots.collectFirst {
      case (id, Bot(values)) if values.toSet == Set(61, 17) => id
    }

    println(soughtBot)

    val soughtOutputs = (0 to 2).toSet.flatMap(finalState.outputs.get).product
    println(soughtOutputs)
  }
}

case class ProgramState(bots: Map[Int, Bot], outputs: Map[Int, Int]) {
  val isAvailable: Instruction => Boolean = {
    case Give(botId, _, _) => bots.get(botId).exists(_.values.length == 2)
    case _ => true
  }

  val updated: Instruction => ProgramState = {
    case Give(botId, lowReceiver, highReceiver) =>
      val bot = bots(botId)

      val updatedLow = updateTarget(bot.values.min, lowReceiver)
      updatedLow.updateTarget(bot.values.max, highReceiver)

    case ValueInput(value, botId) => updateBot(botId, value)
  }

  private def updateTarget(value: Int, target: Receiver) = (target: @unchecked) match {
    case Receiver("bot", botId) => updateBot(target.receiverId, value)
    case Receiver("output", outputId) => copy(outputs = outputs + (outputId -> value))
  }

  private def updateBot(botId: Int, value: Int) = {
    val newBotValue = Bot(value :: bots.get(botId).map(_.values).getOrElse(Nil))
    copy(bots = bots + (botId -> newBotValue))
  }
}

case class Bot(values: List[Int]) extends AnyVal

sealed trait Instruction extends Product with Serializable

case class ValueInput(value: Int, botId: Int) extends Instruction

case class Give(fromBotId: Int, low: Receiver, high: Receiver) extends Instruction

case class Receiver(receiverType: String, receiverId: Int)
