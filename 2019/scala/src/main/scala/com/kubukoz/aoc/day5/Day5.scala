package com.kubukoz.aoc.day5

import cats.effect.IOApp
import cats.effect.{ExitCode, IO}
import cats.effect.Sync
import cats.effect.Console.io._
import cats.implicits._
import cats.data.NonEmptyList
import monocle.macros.Lenses
import monocle.function.Index
import monocle.Optional
import com.kubukoz.aoc.day5.data.Instruction.Combine.Way
import com.kubukoz.aoc.Util
import com.olegpy.meow.prelude._
import com.olegpy.meow.effects._
import cats.mtl.MonadState
import com.kubukoz.aoc.day5.data.Program
import cats.effect.concurrent.Ref

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
      case Token.Add(modes) :: from1 :: from2 :: to :: _ =>
        combine(modes, from1, from2, to, Combine.Way.Add)
      case Token.Mult(modes) :: from1 :: from2 :: to :: _ =>
        combine(modes, from1, from2, to, Combine.Way.Mult)
      case Token.Save :: at :: _          => Save(toPosition(at))
      case Token.Load(modes) :: from :: _ => Load(from.withMode(modes.values(0)))
    }

    private def combine(modes: Modes, from1: Token, from2: Token, to: Token, merge: Combine.Way) =
      Instruction.Combine(
        NonEmptyList.of(
          from1.withMode(modes.values(0)),
          from2.withMode(modes.values(1))
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

  sealed trait Mode extends Product with Serializable

  object Mode {
    case object Positional extends Mode
    case object Immediate  extends Mode
  }

  final case class Modes(values: List[Mode])

  object Token {
    val Halt = Token(99)

    object Add {
      //opcode 01 / 1
      def unapply(tok: Token): Option[Modes] = ???
    }

    object Mult {
      //opcode 02 / 2
      def unapply(tok: Token): Option[Modes] = ???
    }
    val Save = Token(3)

    object Load {
      //opcode 04 / 4
      def unapply(tok: Token): Option[Modes] = ???
    }

    val parse: String => Token = s => Token(s.trim.toInt)
  }
}

trait Interpreter[F[_]] {
  def runProgram: F[Int]
  def setParams(noun: Int, verb: Int): F[Unit]
}

object Interpreter {

  def fromInput[F[_]: Sync](input: Program): F[Interpreter[F]] = Ref[F].of(input).map { ref =>
    implicit val MS = ref.stateInstance
    statefulInstance[F]
  }

  def fromInputWithParams[F[_]: Sync](input: Program, noun: Int, verb: Int): F[Interpreter[F]] =
    fromInput[F](input).flatTap(_.setParams(noun, verb))

  def statefulInstance[F[_]: Program.MState]: Interpreter[F] = new Interpreter[F] {
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

    val pop: F[Token] = State.inspect(Program.stack.get(_)).map(_.head) <*
      State.modify(Program.stack.modify(_.tail))

    def jumpTo(position: Option[Position]): F[Unit] =
      State.modify(Program.nextPosition.set(position))

    def nextPosition(steps: Int) = State.inspect { prog =>
      prog.nextPosition.map(_.position + steps).filter(_ < prog.tokens.length).map(Position)
    }

    def jumpBy(steps: Int): F[Unit] = nextPosition(steps).flatMap(jumpTo)

    val runOp: Instruction => F[Unit] = {
      case Instruction.Halt => State.modify(Program.nextPosition.set(None))
      case Instruction.Load(from) =>
        dereference(from).flatMap(push) *> jumpBy(2)
      case Instruction.Combine(from, to, combine) =>
        val reduce: (Token, Token) => Token = combine match {
          case Way.Add  => _ add _
          case Way.Mult => _ mult _
        }

        for {
          newValue <- from.nonEmptyTraverse(dereference).map(_.reduceLeft(reduce))
          _        <- setAtPosition(to, newValue)
          _        <- jumpBy(4)
        } yield ()
    }

    val getOutput: F[Int] = State.inspect(_.tokens.head.token)

    val runProgram: F[Int] = nextOp.flatMap(runOp).whileM_(running) *> getOutput

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
      val parsed = parse(file)
      part1(parsed).flatMap(putStrLn(_)) *>
        part2(parsed).flatMap(putStrLn(_))
    } as ExitCode.Success

  def parse(input: String): Program = {
    val tokens = input.split(",").toList.map(Token.parse)
    Program(tokens, Position(0).some, Nil)
  }

  def part1(input: Program): IO[Int] =
    Interpreter.fromInputWithParams[IO](input, 12, 2).flatMap(_.runProgram)

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
