import monocle.macros.Lenses
import scala.util.Random
import cats.data.NonEmptyList
import com.kubukoz.aoc._
import cats.implicits._
import cats.data._

sealed trait Operation

object Operation {
  case object Acc extends Operation
  case object Jmp extends Operation
  case object Nop extends Operation

  val values = List(Acc, Jmp, Nop)
}

case class Instruction(op: Operation, arg: Int)

def instruction(line: String) = line match {
  case s"$op $arg" =>
    Instruction(Operation.values.find(_.toString.equalsIgnoreCase(op)).get, arg.toInt)
}

@Lenses
case class Machine(acc: Int, pos: Int, history: Set[Int], instructions: List[Instruction])

object Machine {
  val get: State[Machine, Machine] = State.get

  def modify(f: Machine => Machine): State[Machine, Unit] = State.modify(f)

  def nextInstruction: State[Machine, Option[Instruction]] = Machine.get.map {
    case oldMachine if oldMachine.history.contains(oldMachine.pos) => None
    case oldMachine                                                => oldMachine.instructions(oldMachine.pos).some
  }

  def addAccumulator(add: Int): State[Machine, Unit] = modify(Machine.acc.modify(_ + add))
  def jump(steps: Int): State[Machine, Unit] = modify(Machine.pos.modify(_ + steps))
  def log(position: Int): State[Machine, Unit] = modify(Machine.history.modify(_ + position))

  def interpret(instruction: Instruction): State[Machine, Unit] = {
    val theArg = instruction.arg

    val action = instruction.op match {
      case Operation.Acc => Machine.addAccumulator(theArg) *> Machine.jump(1)
      case Operation.Jmp => Machine.jump(theArg)
      case Operation.Nop => Machine.jump(1)
    }

    Machine.get.map(_.pos).flatMap(Machine.log) *> action
  }

}

val instructions = Util
  .readFileUnsafe("./files/day8.txt")
  .map(instruction)

def run =
  OptionT(Machine.nextInstruction)
    .semiflatMap(Machine.interpret)
    .foreverM
    .value
    .runS(Machine(0, 0, Set.empty, instructions))
    .value

def part1 = run.acc
