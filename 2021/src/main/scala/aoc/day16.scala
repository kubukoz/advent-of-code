package aoc

import cats.mtl.Stateful

import cats.data.StateT

import cats.MonadError

import cats.StackSafeMonad

import java.lang
import cats.implicits._
import aoc.lib._

object Day16 extends App {

  val charToBit: Char => Bit = {
    case '0' => 0
    case '1' => 1
  }

  val key =
    """0 = 0000
       1 = 0001
       2 = 0010
       3 = 0011
       4 = 0100
       5 = 0101
       6 = 0110
       7 = 0111
       8 = 1000
       9 = 1001
       A = 1010
       B = 1011
       C = 1100
       D = 1101
       E = 1110
       F = 1111"""
      .split("\n")
      .map(_.trim)
      .map { case s"$ch = $encoding" => ch.head -> encoding.map(charToBit) }
      .toMap

  sealed trait Bit extends Product with Serializable {
    def isOne = this == _1

    def toChar =
      this match {
        case `_1` => '1'
        case `_0` => '0'
      }

  }

  case object _0 extends Bit
  case object _1 extends Bit

  object Bit {
    implicit def fromOne(one: 1): Bit = _1
    implicit def fromZero(zero: 0): Bit = _0
  }

  case class EpsilonError(msg: String, index: Int) extends Throwable(msg + " at index " + index)

  case class ParserState[A](bits: Vector[A], index: Int) {

    def proceed: Either[EpsilonError, (A, ParserState[A])] =
      if (index < bits.length)
        Right((bits(index), copy(index = index + 1)))
      else
        Left(EpsilonError("No more bits", index))

  }

  sealed trait Parser[+A] {

    def takeThrough(p: A => Boolean): Parser[List[A]] = List.empty[A].tailRecM { memory =>
      this.map { a =>
        if (p(a))
          Left(a :: memory)
        else
          Right((a :: memory).reverse)
      }
    }

    def parse(s: Vector[Bit]): Either[EpsilonError, A] = Parser
      .compile[StateT[Either[EpsilonError, *], ParserState[Bit], *], A](this)
      .runA(ParserState(s, 0))

    def parseUnsafe(s: Vector[Bit]): A = parse(s).toTry.get

  }

  object Parser {

    def compile[F[_]: MonadError[*[_], EpsilonError], A](
      p: Parser[A]
    )(
      implicit S: Stateful[F, ParserState[Bit]]
    ): F[A] =
      p match {
        // free stuff
        case Pure(a)          => a.pure[F]
        case Raise(e)         => e.raiseError[F, A]
        case f: FlatMap[a, b] => compile[F, a](f.fa).flatMap(a => compile[F, A](f.f(a)))
        case a: Attempt[a]    => compile[F, a](a.fa).attempt.map(identity(_))
        // actual stuff
        case Index => S.inspect(_.index)
        case Bit =>
          S.inspect(_.proceed)
            .rethrow
            .flatMap { case (r, s) => S.set(s).as(r) }
            .map(identity(_))
      }

    case class Pure[A](a: A) extends Parser[A]
    case class FlatMap[A, B](fa: Parser[A], f: A => Parser[B]) extends Parser[B]
    case object Index extends Parser[Int]
    case object Bit extends Parser[Bit]
    case class Raise(e: EpsilonError) extends Parser[Nothing]
    case class Attempt[A](fa: Parser[A]) extends Parser[Either[EpsilonError, A]]

    val unit: Parser[Unit] = Pure(())

    implicit val monad: MonadError[Parser, EpsilonError] =
      new StackSafeMonad[Parser] with MonadError[Parser, EpsilonError] {
        def flatMap[A, B](fa: Parser[A])(f: A => Parser[B]): Parser[B] = FlatMap(fa, f)

        def pure[A](x: A): Parser[A] = Pure(x)

        def raiseError[A](e: EpsilonError): Parser[A] = Raise(e)

        def handleErrorWith[A](
          fa: Parser[A]
        )(
          f: EpsilonError => Parser[A]
        ): Parser[A] = attempt(fa).flatMap(_.fold(f, pure))

        override def attempt[A](fa: Parser[A]): Parser[Either[EpsilonError, A]] = Attempt(fa)

      }

    def const(bits: List[Bit]): Parser[Unit] =
      (nBits(bits.size), Parser.index).mapN {
        case (actual, _) if actual == bits => Right(())
        case (actual, i)                   => Left(EpsilonError(s"Expected $bits, got $actual", i))
      }.rethrow

    def nBits(n: Int): Parser[List[Bit]] = bit.replicateA(n)
    val bit: Parser[Bit] = Bit
    val index: Parser[Int] = Index

    def raiseMessage(msg: String): Parser[Nothing] = index.flatMap(i => Raise(EpsilonError(msg, i)))
  }

  def parseBits(bits: List[Bit]): Int = lang.Integer.parseInt(bits.map(_.toChar).mkString, 2)
  def parseBitsToLong(bits: List[Bit]): Long = lang.Long.parseLong(bits.map(_.toChar).mkString, 2)

  case class Version(value: Long)

  sealed trait Packet extends Product with Serializable {

    def fold[A](literal: (Version, Long) => A, operator: (Version, OpType, List[A]) => A): A = {
      def recurse(packet: Packet) = packet.fold(literal, operator)

      this match {
        case Literal(version, value)     => literal(version, value)
        case Operator(version, op, subs) => operator(version, op, subs.map(recurse))
      }
    }

  }

  case class Operator(version: Version, tpe: OpType, subs: List[Packet]) extends Packet

  case class Literal(version: Version, value: Long) extends Packet

  sealed trait OpType extends Product with Serializable {
    import OpType._

    def eval: (Long, Long) => Long = {
      def cond(f: (Long, Long) => Boolean): (Long, Long) => Long =
        (a, b) =>
          if (f(a, b))
            1L
          else
            0L

      this match {
        case Sum         => _ + _
        case Product     => _ * _
        case Minimum     => _ min _
        case Maximum     => _ max _
        case GreaterThan => cond(_ > _)
        case LessThan    => cond(_ < _)
        case EqualTo     => cond(_ == _)
      }
    }

  }

  object OpType {
    case object Sum extends OpType
    case object Product extends OpType
    case object Minimum extends OpType
    case object Maximum extends OpType
    case object GreaterThan extends OpType
    case object LessThan extends OpType
    case object EqualTo extends OpType

    val values = Map(
      0 -> Sum,
      1 -> Product,
      2 -> Minimum,
      3 -> Maximum,
//  4 -> Sike
      5 -> GreaterThan,
      6 -> LessThan,
      7 -> EqualTo,
    )

  }

  object parsers {
    import Parser._
    val version = nBits(3).map(parseBits(_)).map(Version(_))

    val literal: Parser[Literal] = {
      val content = nBits(5).takeThrough(_.head.isOne).map(_.map(_.tail).flatten)

      (
        version,
        const(List(1, 0, 0)) *>
          content.map(parseBitsToLong),
      ).mapN(Literal.apply)
    }

    def parseType(i: Int): Parser[OpType] =
      OpType
        .values
        .get(i)
        .fold[Parser[OpType]](Parser.raiseMessage(s"Unknown op type: ${i}"))(_.pure[Parser])

    val operator: Parser[Packet] =
      (
        version,
        nBits(3).map(parseBits).flatMap(parseType),
        bit.flatMap {
          case `_0` =>
            nBits(15).map(parseBits).flatMap { subpacketsLength =>
              index.flatMap { indexBefore =>
                val targetIndex = indexBefore + subpacketsLength

                packet.whileM[List](Parser.index.map(_ < targetIndex))
              }
            }

          case `_1` =>
            nBits(11).map(parseBits).flatMap { subpacketCount =>
              packet.replicateA(subpacketCount)
            }
        },
      ).mapN(Operator.apply)

    lazy val packet = literal.widen[Packet].orElse(operator)

  }

  val realInput = readAll("day16.txt")
  val literal = "D2FE28"

  def toBin(input: String) = input.flatMap(key(_)).toVector

// parsers.literal.parseUnsafe(toBin(literal)).valueDec

// parsers.operator.parseUnsafe(toBin("38006F45291200"))
  val data = toBin(realInput)

  def sumVersions(packet: Packet) = packet.fold[Long](
    literal = (version, _) => version.value,
    operator = (version, _, children) => version.value |+| children.combineAll,
  )

  def eval(packet: Packet) = packet.fold[Long](
    literal = (_, value) => value,
    operator = (_, op, children) => children.reduceLeft(op.eval),
  )

  val parsed = parsers.packet.parseUnsafe(data)

  assertEquals(sumVersions(parsed), 947, "Part 1")
  assertEquals(eval(parsed), 660797830937L, "Part 2")
}
