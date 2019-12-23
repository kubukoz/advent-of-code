package com.kubukoz.aoc.day5

import cats.effect.Console.io._
import cats.effect.Console.implicits._
import cats.effect.Console
import cats.effect.concurrent.Ref
import cats.effect.{ExitCode, IO, IOApp, Sync}
import cats.implicits._
import cats.mtl.{ApplicativeAsk, MonadState}
import cats.tagless.autoFunctorK
import cats.~>
import com.kubukoz.aoc.Util
import com.kubukoz.aoc.day5.data.Instruction.{BinOp, UnaryOp}
import com.kubukoz.aoc.day5.data.{Debug, Program, Token}
import com.olegpy.meow.effects._
import com.olegpy.meow.prelude._
import monocle.Optional
import monocle.function.Index
import monocle.macros.Lenses

private[day5] object data {

  type Debug[F[_]] = ApplicativeAsk[F, Boolean]
  def Debug[F[_]](implicit F: Debug[F]): F[Boolean] = F.ask

  @Lenses
  final case class Program(tokens: List[Token], nextPosition: Option[Position], stack: List[Token])

  final case class Position(position: Int)

  object Program {
    type MState[F[_]] = MonadState[F, Program]

    def tokenAt(position: Int): Optional[Program, Token] =
      tokens.composeOptional(Index.index(position))
  }

  sealed trait Instruction extends Product with Serializable

  //noinspection ZeroIndexToHead
  object Instruction {
    case object Halt extends Instruction

    final case class Save(to: Position)    extends Instruction
    final case class Load(from: Reference) extends Instruction

    final case class JumpIf(ref: Reference, location: Reference, check: UnaryOp[Boolean])
        extends Instruction

    final case class BinaryMatch(a: Reference, b: Reference, to: Position, combine: BinOp)
        extends Instruction

    val decode: PartialFunction[List[Token], Instruction] = {
      case Token.Halt :: _ => Halt

      case Token.header(Token.Add, modes) :: from1 :: from2 :: to :: _ =>
        combine(modes, from1, from2, to, BinOp.Add)

      case Token.header(Token.Mult, modes) :: from1 :: from2 :: to :: _ =>
        combine(modes, from1, from2, to, BinOp.Mult)

      case Token.header(Token.Save, _) :: at :: _ => Save(toPosition(at))

      case Token.header(Token.Load, modes) :: from :: _ => Load(from.withMode(modes(0)))

      case Token.header(Token.JumpIfTrue, modes) :: operand :: target :: _ =>
        JumpIf(
          operand.withMode(modes(0)),
          target.withMode(modes(1)),
          UnaryOp.Inverse(UnaryOp.IsZero)
        )

      case Token.header(Token.JumpIfFalse, modes) :: operand :: target :: _ =>
        JumpIf(operand.withMode(modes(0)), target.withMode(modes(1)), UnaryOp.IsZero)

      case Token.header(Token.LessThan, modes) :: a :: b :: store :: _ =>
        combine(modes, a, b, store, BinOp.LessThan)

      case Token.header(Token.Equals, modes) :: a :: b :: target :: _ =>
        combine(modes, a, b, target, BinOp.EqualTo)
    }

    private def combine(
      modes: List[Mode],
      from1: Token,
      from2: Token,
      to: Token,
      merge: BinOp
    ) =
      Instruction.BinaryMatch(
        from1.withMode(modes(0)),
        from2.withMode(modes(1)),
        toPosition(to),
        merge
      )

    sealed trait UnaryOp[A] extends Product with Serializable

    object UnaryOp {
      final case class Inverse(underlying: UnaryOp[Boolean]) extends UnaryOp[Boolean]
      case object IsZero                                     extends UnaryOp[Boolean]

      def toFunction[A]: UnaryOp[A] => Token => A = {
        case Inverse(op) => toFunction(op).map(!_)
        case IsZero      => _.token === 0
      }
    }

    sealed trait BinOp extends Product with Serializable

    object BinOp {

      val toFunction: BinOp => (Token, Token) => Token = {
        case BinOp.Add      => _ add _
        case BinOp.Mult     => _ mult _
        case BinOp.LessThan => (a, b) => if (a.token < b.token) Token(1) else Token(0)
        case BinOp.EqualTo  => (a, b) => if (a.token == b.token) Token(1) else Token(0)
      }

      case object Add      extends BinOp
      case object Mult     extends BinOp
      case object LessThan extends BinOp
      case object EqualTo  extends BinOp
    }
  }

  val toPosition: Token => Position = t => Position(t.token)

  sealed trait Reference extends Product with Serializable

  object Reference {
    final case class ByPosition(pos: Position) extends Reference
    final case class ByValue(value: Int)       extends Reference
  }

  final case class Token(token: Int) {

    val withMode: Mode => Reference = {
      case Mode.Positional => Reference.ByPosition(Position(token))
      case Mode.Immediate  => Reference.ByValue(token)
    }

    def add(another: Token): Token  = Token(token + another.token)
    def mult(another: Token): Token = Token(token * another.token)
  }

  object Token {
    val parse: String => Token = s => Token(s.toInt)

    object header {

      def unapply(token: Token): Some[(Token, List[Mode])] = {
        val h = CommandHeader.parser(token.token.toString)
        Some((h.kind, h.modes))
      }

    }

    val Halt: Token        = Token(99)
    val Add: Token         = Token(1)
    val Mult: Token        = Token(2)
    val Save: Token        = Token(3)
    val Load: Token        = Token(4)
    val JumpIfTrue: Token  = Token(5)
    val JumpIfFalse: Token = Token(6)
    val LessThan: Token    = Token(7)
    val Equals: Token      = Token(8)
  }

  sealed trait Mode extends Product with Serializable

  object Mode {
    case object Positional extends Mode
    case object Immediate  extends Mode
  }

  final case class CommandHeader(modes: List[Mode], kind: Token)

  object CommandHeader {

    val parser: String => CommandHeader = { s =>
      val modes = s
        .dropRight(2)
        .map {
          case '0' => Mode.Positional
          case '1' => Mode.Immediate
          case e   => throw new Exception(s"Failed to parse mode from [$e] for token [$s]")
        }
        .toList
        .reverse
        .padTo(3, Mode.Positional)

      val cmd = Token(s.takeRight(2).last.toString.toInt)

      CommandHeader(modes, cmd)
    }
  }
}

@autoFunctorK
trait Interpreter[F[_]] {
  def popStack: F[Token]
  def push(token: Token): F[Unit]
  def runProgram: F[Int]
}

object Interpreter {

  def fromInput[F[_]: Sync: Console: Debug](input: Program): F[Interpreter[F]] =
    Ref[F].of(input).map { ref =>
      import cats.tagless.implicits._
      implicit val MS = ref.stateInstance

      val handleFailure = λ[F ~> F](
        _.onError {
          case _ =>
            Console[F].putError("Failure! Dumping data") *>
              MS.get.flatMap { state =>
                Console[F].putError("Program: " + state.tokens.map(_.token)) *>
                  Console[F].putError("Stack: " + state.stack.map(_.token)) *>
                  Console[F].putError("Instruction pointer: " + state.nextPosition)
              }
        }
      )

      statefulInstance[F].mapK(handleFailure)
    }

  def statefulInstance[F[_]: Program.MState: Console: Debug]: Interpreter[F] = new Interpreter[F] {
    val State: Program.MState[F] = implicitly[Program.MState[F]]
    import data._

    val running: F[Boolean] = State.inspect(_.nextPosition.nonEmpty)

    val nextOp: F[Instruction] =
      State.inspect(s => Instruction.decode(s.tokens.drop(s.nextPosition.foldMap(_.position))))

    def atPosition(pos: Position): F[Token] =
      State.inspect(
        Program
          .tokenAt(pos.position)
          .getOption(_)
          .getOrElse(throw new Exception(s"No token at position $pos"))
      )

    def setAtPosition(pos: Position, token: Token): F[Unit] =
      State.modify(Program.tokenAt(pos.position).set(token))

    val dereference: Reference => F[Token] = {
      case Reference.ByValue(v)      => Token(v).pure[F]
      case Reference.ByPosition(pos) => atPosition(pos)
    }

    def push(token: Token): F[Unit] = State.modify(Program.stack.modify(token :: _))

    val popStack: F[Token] = State.inspect(Program.stack.get(_)).map(_.head) <*
      State.modify(Program.stack.modify(_.tail))

    def jumpTo(position: Option[Position]): F[Unit] =
      State.modify(Program.nextPosition.set(position))

    def nextPosition(steps: Int): F[Option[Position]] = State.inspect { prog =>
      prog.nextPosition.map(_.position + steps).filter(_ < prog.tokens.length).map(Position)
    }

    def jumpBy(steps: Int): F[Unit] = nextPosition(steps).flatMap(jumpTo)

    val runOp: Instruction => F[Unit] = {
      case Instruction.Halt       => jumpTo(none)
      case Instruction.Load(from) => dereference(from).flatMap(push) *> jumpBy(2)
      case Instruction.Save(to)   => popStack.flatMap(setAtPosition(to, _)) *> jumpBy(2)

      case Instruction.JumpIf(ref, to, check) =>
        dereference(ref)
          .map(UnaryOp.toFunction(check))
          .ifM(
            ifTrue = dereference(to).map(toPosition(_).some).flatMap(jumpTo),
            ifFalse = jumpBy(3)
          )

      case Instruction.BinaryMatch(a, b, to, combine) =>
        combineImpl(a, b, to, combine) *> jumpBy(4)
    }

    private def combineImpl(a: Reference, b: Reference, to: Position, combine: BinOp): F[Unit] =
      (dereference(a), dereference(b)).mapN(BinOp.toFunction(combine)).flatMap(setAtPosition(to, _))

    val getOutput: F[Int] = State.inspect(_.tokens.head.token)

    private def dumpInstruction(debug: Boolean): Instruction => F[Unit] =
      op => Console[F].putStrLn("Running operation " + op).whenA(debug)

    private def dumpStack(debug: Boolean): F[Unit] =
      State
        .inspect(_.stack)
        .map("Stack dump: " + _.toString)
        .flatMap(Console[F].putStrLn(_))
        .whenA(debug)

    val runProgram: F[Int] = Debug[F].flatMap { debug =>
      val runLoop = nextOp.flatTap(dumpInstruction(debug)).flatMap(runOp) <* dumpStack(debug)

      runLoop.whileM_(running)
    } *> getOutput
  }
}

object Day5 extends IOApp {

  implicit val debug: Debug[IO] = ApplicativeAsk.const(false)

  import data._

  def run(args: List[String]): IO[ExitCode] =
    Util.readFile[IO]("files/day5.txt").flatMap { file =>
      val parsed = parse(file)

      (part1(parsed), part2(parsed)).tupled.map(_.toString).flatMap(putStrLn)
    } as ExitCode.Success

  def parse(input: String): Program = {
    val tokens = input.split(",").map(_.trim).toList.map(Token.parse)
    Program(tokens, Position(0).some, Nil)
  }

  def part1(input: Program): IO[Token] =
    Interpreter
      .fromInput[IO](input)
      .flatTap(_.push(Token(1)))
      .flatTap(_.runProgram)
      .flatMap(_.popStack)

  def part2(input: Program): IO[Token] =
    Interpreter
      .fromInput[IO](input)
      .flatTap(_.push(Token(5)))
      .flatTap(_.runProgram)
      .flatMap(_.popStack)

}
