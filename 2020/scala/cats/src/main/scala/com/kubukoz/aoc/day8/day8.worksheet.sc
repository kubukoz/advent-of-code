import scala.util.control.NonFatal
import cats.Monad
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

  val values = NonEmptyList.of(Acc, Jmp, Nop)

  val variate: Operation => Option[Operation] = {
    case Nop => Jmp.some
    case Jmp => Nop.some
    case Acc => None
  }

}

@Lenses
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
    case oldMachine                                                => oldMachine.instructions.get(oldMachine.pos.toLong)
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

def recurseVariate[A, S](
  as: State[S, Option[A]]
)(
  variate: A => Option[A]
)(
  perform: A => State[S, Unit]
)(
  init: S
): List[S] = {
  val (machineBeforeFork, nextInstruction) = as.run(init).value

  nextInstruction match {
    /* all done, returning machine */
    case None              => List(machineBeforeFork)
    case Some(instruction) =>
      val variation: Option[A] = variate(instruction)

      val next = recurseVariate(as)(variate)(perform)(perform(instruction).runS(machineBeforeFork).value)

      variation match {
        case Some(theVariation) =>
          next ++ recurseVariate(as)(_ => None)(perform)(perform(theVariation).runS(machineBeforeFork).value)
        case None               => next
      }
  }
}

def run(variate: Instruction => Option[Instruction]): List[Machine] =
  recurseVariate(Machine.nextInstruction)(variate)(Machine.interpret)(
    Machine(0, 0, Set.empty, instructions)
  )

val part1 = run(_ => None).map(_.acc)

val part2 = run(Instruction.op.modifyF(Operation.variate))
  .filter { machine =>
    machine.pos == machine.instructions.length
  }
  .map(_.acc)
