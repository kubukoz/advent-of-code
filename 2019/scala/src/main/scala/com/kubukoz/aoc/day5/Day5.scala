package com.kubukoz.aoc.day5

import cats.data.NonEmptyList
import cats.effect.Console.io._
import cats.effect.Console.implicits._
import cats.effect.Console
import cats.effect.concurrent.Ref
import cats.effect.{ExitCode, IO, IOApp, Sync}
import cats.implicits._
import cats.mtl.MonadState
import com.kubukoz.aoc.Util
import com.kubukoz.aoc.day5.data.Instruction.Combine.Way
import com.kubukoz.aoc.day5.data.{Program, Token}
import com.olegpy.meow.effects._
import com.olegpy.meow.prelude._
import monocle.Optional
import monocle.function.Index
import monocle.macros.Lenses

private[day5] object data {

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

    final case class Combine(from: NonEmptyList[Reference], to: Position, combine: Combine.Way)
        extends Instruction

    final case class Save(to: Position)    extends Instruction
    final case class Load(from: Reference) extends Instruction

    object Combine {
      sealed trait Way extends Product with Serializable

      object Way {
        case object Add  extends Way
        case object Mult extends Way
      }
    }

    val decode: PartialFunction[List[Token], Instruction] = {
      case Token.Halt :: _ => Halt
      case Token.header(Token.Add, modes) :: from1 :: from2 :: to :: _ =>
        combine(modes, from1, from2, to, Combine.Way.Add)
      case Token.header(Token.Mult, modes) :: from1 :: from2 :: to :: _ =>
        combine(modes, from1, from2, to, Combine.Way.Mult)
      case Token.header(Token.Save, _) :: at :: _       => Save(toPosition(at))
      case Token.header(Token.Load, modes) :: from :: _ => Load(from.withMode(modes(0)))
    }

    private def combine(
      modes: List[Mode],
      from1: Token,
      from2: Token,
      to: Token,
      merge: Combine.Way
    ) =
      Instruction.Combine(
        NonEmptyList.of(
          from1.withMode(modes(0)),
          from2.withMode(modes(1))
        ),
        toPosition(to),
        merge
      )
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

    val Halt: Token = Token(99)
    val Add: Token  = Token(1)
    val Mult: Token = Token(2)
    val Save: Token = Token(3)
    val Load: Token = Token(4)
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
        }
        .toList
        .reverse
        .padTo(3, Mode.Positional)

      val cmd = Token(s.takeRight(2).last.toString.toInt)

      CommandHeader(modes, cmd)
    }
  }
}

trait Interpreter[F[_]] {
  def getStackHead: F[Token]
  def push(token: Token): F[Unit]
  def runProgram: F[Int]
  def setParams(noun: Int, verb: Int): F[Unit]
}

object Interpreter {

  def fromInput[F[_]: Sync: Console](input: Program): F[Interpreter[F]] = Ref[F].of(input).map {
    ref =>
      implicit val MS = ref.stateInstance
      statefulInstance[F]
  }

  def fromInputWithParams[F[_]: Sync: Console](
    input: Program,
    noun: Int,
    verb: Int
  ): F[Interpreter[F]] =
    fromInput[F](input).flatTap(_.setParams(noun, verb))

  def statefulInstance[F[_]: Program.MState: Console]: Interpreter[F] = new Interpreter[F] {
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
      State.modify(a => Program.tokenAt(pos.position).set(token)(a))

    val dereference: Reference => F[Token] = {
      case Reference.ByValue(v)      => Token(v).pure[F]
      case Reference.ByPosition(pos) => atPosition(pos)
    }

    val getStackHead: F[Token] = State.inspect(Program.stack.get(_).head)

    def push(token: Token): F[Unit] = State.modify(Program.stack.modify(token :: _))

    val pop: F[Token] = State.inspect(Program.stack.get(_)).map(_.head) <*
      State.modify(Program.stack.modify(_.tail))

    def jumpTo(position: Option[Position]): F[Unit] =
      State.modify(Program.nextPosition.set(position))

    def nextPosition(steps: Int): F[Option[Position]] = State.inspect { prog =>
      prog.nextPosition.map(_.position + steps).filter(_ < prog.tokens.length).map(Position)
    }

    def jumpBy(steps: Int): F[Unit] = nextPosition(steps).flatMap(jumpTo)

    val runOp: Instruction => F[Unit] = {
      case Instruction.Halt       => State.modify(Program.nextPosition.set(None))
      case Instruction.Load(from) => dereference(from).flatMap(push) *> jumpBy(2)
      case Instruction.Save(to)   => pop.flatMap(setAtPosition(to, _)) *> jumpBy(2)

      case Instruction.Combine(from, to, combine) =>
        val reduce: (Token, Token) => Token = combine match {
          case Way.Add  => _ add _
          case Way.Mult => _ mult _
        }

        for {
          newValue <- from.traverse(dereference).map(_.reduceLeft(reduce))
          _        <- setAtPosition(to, newValue)
          _        <- jumpBy(4)
        } yield ()
    }

    val getOutput: F[Int] = State.inspect(_.tokens.head.token)

    val runProgram: F[Int] = (nextOp
      .flatTap(op => Console[F].putStrLn("Next op is " + op)))
      .flatMap(runOp)
      .whileM_(running) *> getOutput

    def setParams(noun: Int, verb: Int): F[Unit] = {
      val before =
        Program.tokens
          .composeOptional(Index.index(1))
          .set(Token(noun))
          .compose(Program.tokens.composeOptional(Index.index(2)).set(Token(verb)))

      State.modify(before)
    }

  }
}

object Day5 extends IOApp {

  import data._

  def run(args: List[String]): IO[ExitCode] =
    Util.readFile[IO]("files/day5.txt").flatMap { file =>
//      val parsed = parse("3,0,4,0,99")
      val parsed = parse(file)

      part1(parsed).map(_.toString).flatMap(putStrLn)
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
      .flatMap(_.getStackHead)

  def part2(input: Program): IO[Option[Int]] = {
    val range = fs2.Stream.range(0, 100)

    (range, range).tupled.evalMap {
      case (noun, verb) =>
        Interpreter
          .fromInputWithParams[IO](input, noun, verb)
          .flatMap(_.runProgram)
          .map((noun, verb, _))
    }.collectFirst {
      case (noun, verb, 19690720) => 100 * noun + verb
    }.compile.last
  }

}
