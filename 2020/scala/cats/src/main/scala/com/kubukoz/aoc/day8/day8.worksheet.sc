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

case class Machine(acc: Int, pos: Int, history: Set[Int], instructions: List[Instruction])

val nextInstruction: State[Machine, Option[Instruction]] = State.get[Machine].map {
  case oldMachine if oldMachine.history.contains(oldMachine.pos) => None
  case oldMachine                                                => oldMachine.instructions(oldMachine.pos).some
}

def perform(instruction: Instruction): State[Machine, Unit] = {
  val theArg = instruction.arg

  val next = State.modify[Machine](m => m.copy(pos = m.pos + 1))

  val action: State[Machine, Unit] = instruction.op match {
    case Operation.Acc => State.modify[Machine](m => m.copy(acc = m.acc + theArg)) *> next
    case Operation.Jmp => State.modify[Machine](m => m.copy(pos = m.pos + theArg))
    case Operation.Nop => next
  }

  State.get[Machine].map(_.pos).flatMap { thePos =>
    action *> State.modify(m => m.copy(history = m.history + thePos))
  }
}

val instructions = Util
  .readFileUnsafe("./files/day8.txt")
  .map(instruction)

def run =
  OptionT(nextInstruction).semiflatMap(perform).foreverM.value.runS(Machine(0, 0, Set.empty, instructions)).value

run.acc
// OptionT(perform(instructions)).foreverM.value.runS(Machine(0, 0, Nil)).value

// import cats.effect.IO
// import cats.effect.unsafe.implicits._

// val dupa = OptionT(IO(println("dupa")) *> IO(if (Random.nextBoolean()) Some(42) else None))

// dupa.foreverM.value.unsafeRunSync()
