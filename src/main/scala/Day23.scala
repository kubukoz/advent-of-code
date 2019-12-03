

object Day23 {
  val input =
    """jio a, +22
      |inc a
      |tpl a
      |tpl a
      |tpl a
      |inc a
      |tpl a
      |inc a
      |tpl a
      |inc a
      |inc a
      |tpl a
      |inc a
      |inc a
      |tpl a
      |inc a
      |inc a
      |tpl a
      |inc a
      |inc a
      |tpl a
      |jmp +19
      |tpl a
      |tpl a
      |tpl a
      |tpl a
      |inc a
      |inc a
      |tpl a
      |inc a
      |tpl a
      |inc a
      |inc a
      |tpl a
      |inc a
      |inc a
      |tpl a
      |inc a
      |tpl a
      |tpl a
      |jio a, +8
      |inc b
      |jie a, +4
      |tpl a
      |inc a
      |jmp +2
      |hlf a
      |jmp -7""".stripMargin.split("\n")
  val hlfPattern = "hlf (.+)".r
  val tplPattern = "tpl (.+)".r
  val incPattern = "inc (.+)".r
  val jmpPattern = "jmp ([-|+]?[0-9]+)".r
  val jiePattern = "jie (.+), ([-|+]?[0-9]+)".r
  val jioPattern = "jio (.+), ([-|+]?[0-9]+)".r

  val commands: Seq[Either[Command, Condition]] = input.map {
    case hlfPattern(reg) => Left(Halfen(Reg(reg)))
    case tplPattern(reg) => Left(Triple(Reg(reg)))
    case incPattern(reg) => Left(Increment(Reg(reg)))
    case jmpPattern(steps) => Right(Always(steps.toInt))
    case jiePattern(reg, steps) => Right(Even(Reg(reg), steps.toInt))
    case jioPattern(reg, steps) => Right(One(Reg(reg), steps.toInt))
  }

  def runCircuit(a: Int = 0, b: Int = 0, currentInstruction: Int = 0): (Int, Int) = currentInstruction match {
    case _ if currentInstruction == commands.size => (a, b)
    case _ =>
      commands(currentInstruction) match {
      case Left(Halfen(reg)) => reg match {
        case A => runCircuit(a / 2, b, currentInstruction + 1)
        case B => runCircuit(a, b / 2, currentInstruction + 1)
      }
      case Left(Triple(reg)) => reg match {
        case A => runCircuit(a * 3, b, currentInstruction + 1)
        case B => runCircuit(a, b * 3, currentInstruction + 1)
      }
      case Left(Increment(reg)) => reg match {
        case A => runCircuit(a + 1, b, currentInstruction + 1)
        case B => runCircuit(a, b + 1, currentInstruction + 1)
      }
      case Right(Even(reg, steps)) => reg match{
        case A => runCircuit(a, b, currentInstruction + (if(a % 2 == 0) steps else 1))
        case B => runCircuit(a, b, currentInstruction + (if(b % 2 == 0) steps else 1))
      }
      case Right(One(reg, steps)) => reg match{
        case A => runCircuit(a, b, currentInstruction + (if(a == 1) steps else 1))
        case B => runCircuit(a, b, currentInstruction + (if(b == 1) steps else 1))
      }
      case Right(Always(steps)) => runCircuit(a, b, currentInstruction + steps)
    }
  }

  def main(args: Array[String]) {
    println("Part 1, b: " + runCircuit()._2)
    println("Part 2, b: " + runCircuit(1)._2)
  }

  sealed case class Reg(name: String)

  object A extends Reg("a")

  object B extends Reg("b")

  object NoReg extends Reg("")

  sealed class Condition(reg: Reg, ifTrue: Int, ifFalse: Int)

  case class Always(value: Int) extends Condition(NoReg, value, value)

  case class Even(reg: Reg, ifTrue: Int) extends Condition(reg, ifTrue, 0)

  case class One(reg: Reg, ifTrue: Int) extends Condition(reg, ifTrue, 0)

  sealed class Command(reg: Reg)

  case class Halfen(reg: Reg) extends Command(reg)

  case class Triple(reg: Reg) extends Command(reg)

  case class Increment(reg: Reg) extends Command(reg)

}
