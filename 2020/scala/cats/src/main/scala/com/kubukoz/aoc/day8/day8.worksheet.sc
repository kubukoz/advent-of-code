import cats.Monad
import monocle.macros.Lenses
import cats.data.NonEmptyList
import com.kubukoz.aoc._
import cats.implicits._
import cats.data._
import cats.~>

sealed trait Operation

object Operation {
  case object Acc extends Operation
  case object Jmp extends Operation
  case object Nop extends Operation

  val values = NonEmptyList.of(Acc, Jmp, Nop)

  val variate: PartialFunction[Operation, Operation] = {
    case Nop => Jmp
    case Jmp => Nop
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

//Run a state monad in isolation - restore the state seen before it after we have a result.
def isolateK[S]: State[S, *] ~> State[S, *] = new (State[S, *] ~> State[S, *]) {

  def apply[A](fa: State[S, A]): State[S, A] = State { init =>
    val result = fa.runA(init).value

    (init, result)
  }

}

def recurseVariate[F[_]: Monad, A, S](
  getNext: F[Option[A]]
)(
  get: F[S]
)(
  perform: A => F[Unit]
)(
  isolate: F ~> F
)(
  variate: A => Option[A]
): F[List[S]] = {
  val rec = recurseVariate(getNext)(get)(perform)(isolate) _

  getNext.flatMap {
    case None              => get.map(List(_))
    case Some(instruction) =>
      val next = perform(instruction) *> rec(variate)
      val variationNext = variate(instruction).map(perform(_) *> rec(_ => None))

      (next :: variationNext.toList).flatTraverse(isolate(_))
  }
}

def run(variate: Instruction => Option[Instruction]): List[Machine] =
  recurseVariate(Machine.nextInstruction)(State.get)(Machine.interpret)(isolateK)(variate)
    .runA(Machine(0, 0, Set.empty, instructions))
    .value

val part1 = run(_ => None).map(_.acc) //1654

val part2 = run(Instruction.op.modifyF(Operation.variate.lift))
  .filter { machine =>
    machine.pos == machine.instructions.length
  }
  .map(_.acc) //833
