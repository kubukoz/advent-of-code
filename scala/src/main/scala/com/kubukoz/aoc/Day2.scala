package com.kubukoz.aoc

import cats.effect.IOApp
import cats.effect.{ExitCode, IO}
import cats.effect.Sync
import cats.effect.Blocker
import cats.effect.ContextShift
import cats.effect.Console.io._
import cats.implicits._
import java.nio.file.Paths
import io.estatico.newtype.macros.newtype
import cats.data.NonEmptyList
import monocle.macros.Lenses
import cats.data.State
import monocle.function.Index
import monocle.Optional
import com.kubukoz.aoc.data.Instruction.Combine.Way

object Day2 extends IOApp {

  import data._

  type ProgramState[A] = State[Program, A]

  object ProgramState {

    def atPosition(pos: Position): ProgramState[Token] =
      State.inspect(
        Program.tokenAt(pos.position).getOption(_).getOrElse(throw new Exception(s"No token at position $pos"))
      )

    def setAtPosition(pos: Position, token: Token): ProgramState[Unit] =
      State.modify(Program.tokenAt(pos.position).set(token))

    val running: ProgramState[Boolean] = State.inspect(_.nextPosition.nonEmpty)

    val nextOp: ProgramState[Instruction] =
      State.inspect(s => Instruction.decode(s.tokens.drop(s.nextPosition.fold(0)(_.position))))

    val runOp: Instruction => ProgramState[Unit] = {
      case Instruction.Halt => State.modify(Program.nextPosition.set(None))
      case Instruction.Combine(from, to, combine) =>
        val reduce: (Token, Token) => Token = combine match {
          case Way.Add  => _ add _
          case Way.Mult => _ mult _
        }

        val jumpNextPosition: ProgramState[Unit] = State.modify { prog =>
          val nextPosition = prog.nextPosition.map(_.position + 4).flatMap {
            case newPos if newPos < prog.tokens.length => Some(Position(newPos))
            case _                                     => None
          }

          Program.nextPosition.set(nextPosition)(prog)
        }

        for {
          newValue <- from.nonEmptyTraverse(ProgramState.atPosition).map(_.reduceLeft(reduce))
          _        <- ProgramState.setAtPosition(to, newValue)
          _        <- jumpNextPosition
        } yield ()
    }

    val runProgram: State[Program, Unit] =
      ProgramState.nextOp.flatMap(runOp).whileM_(running)

    def runWithParams(noun: Int, verb: Int): State[Program, Unit] = {
      val before =
        Program.tokens
          .composeOptional(Index.index(1))
          .set(Token(noun))
          .compose(Program.tokens.composeOptional(Index.index(2)).set(Token(verb)))

      State.modify(before) *> runProgram
    }
  }

  def run(args: List[String]): IO[ExitCode] =
    Blocker[IO].use { blocker =>
      Util.readFile[IO]("files/day2.txt", blocker).flatMap { file =>
        putStrLn(part1(file)) *>
          putStrLn(part2(file))
      }
    } as ExitCode.Success

  def parse(input: String): Program = {
    val tokens = input.split(",").map(Token.parse).toList
    Program(tokens, Position(0).some)
  }

  def part1(input: String): Int = {
    //they had us in the first half, not gonna lie
    ProgramState.runWithParams(12, 2).runS(parse(input)).map(getOutput).value
  }

  val getOutput: Program => Int = _.tokens.head.token

  def part2(input: String): Option[Int] = {
    val range   = fs2.Stream.range(0, 100)
    val initial = parse(input)

    (range, range).tupled.fproduct {
      case (noun, verb) => ProgramState.runWithParams(noun, verb).runS(initial).map(getOutput).value
    }.collectFirst {
      case ((noun, verb), 19690720) => 100 * noun + verb
    }.compile.last
  }

}

object data {

  @newtype
  final case class Position(position: Int)

  @newtype
  final case class Token(token: Int) {
    def add(another: Token): Token  = Token(token + another.token)
    def mult(another: Token): Token = Token(token * another.token)
  }

  object Token {
    val Halt = Token(99)
    val Add  = Token(1)
    val Mult = Token(2)

    val parse: String => Token = s => Token(s.trim.toInt)
  }

  sealed trait Instruction extends Product with Serializable

  object Instruction {
    case object Halt                                                                           extends Instruction
    final case class Combine(from: NonEmptyList[Position], to: Position, combine: Combine.Way) extends Instruction

    object Combine {
      sealed trait Way extends Product with Serializable

      object Way {
        case object Add  extends Way
        case object Mult extends Way
      }
    }

    val decode: PartialFunction[List[Token], Instruction] = {
      case Token.Halt :: _                         => Instruction.Halt
      case Token.Add :: from1 :: from2 :: to :: _  => combine(from1, from2, to, Combine.Way.Add)
      case Token.Mult :: from1 :: from2 :: to :: _ => combine(from1, from2, to, Combine.Way.Mult)
    }

    private val toPosition: Token => Position = t => Position(t.token)

    private def combine(from1: Token, from2: Token, to: Token, merge: Combine.Way) =
      Instruction.Combine(NonEmptyList.of(from1, from2).map(toPosition), toPosition(to), merge)
  }

  @Lenses
  final case class Program(tokens: List[Token], nextPosition: Option[Position])

  object Program {
    def tokenAt(position: Int): Optional[Program, Token] = tokens.composeOptional(Index.index(position))
  }
}

object Util {

  def readFile[F[_]: Sync: ContextShift](name: String, blocker: Blocker): F[String] =
    fs2.io.file.readAll(Paths.get(name), blocker, 4096).through(fs2.text.utf8Decode[F]).compile.string
}
