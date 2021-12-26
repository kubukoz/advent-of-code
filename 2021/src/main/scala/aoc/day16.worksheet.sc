import cats.mtl.Stateful

import cats.data.StateT

import cats.MonadError

import cats.StackSafeMonad

import java.lang
import cats.implicits._
import aoc.lib._

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
    .map { case s"$ch = $encoding" => ch.head -> encoding.map(charToBit) }
    .toMap

//format:
// version <3 bits> type <3 bits>...

// type 100:
// 101111111000101000
// 10111 11110 00101 000

// other types:
// version <3 bits> type <3 bits> length <1/0> subpackets
// length=0 => next 15 bits are the total length in bits of the nested subpackets
// length=1 => next 11 bits are the # of nested subpackets
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

case class EpislonError(msg: String, index: Int) extends Throwable(msg + " at index " + index)

case class ParserState[A](bits: Vector[A], index: Int) {

  def proceed: Either[EpislonError, (A, ParserState[A])] =
    if (index < bits.length)
      Right((bits(index), copy(index = index + 1)))
    else
      Left(EpislonError("No more bits", index))

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

  def parse(s: Vector[Bit]): Either[EpislonError, A] = Parser
    .compile[StateT[Either[EpislonError, *], ParserState[Bit], *], A](this)
    .runA(ParserState(s, 0))

  def parseUnsafe(s: Vector[Bit]): A = parse(s).toTry.get

}

object Parser {

  def compile[F[_]: MonadError[*[_], EpislonError], A](
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
  case class Raise(e: EpislonError) extends Parser[Nothing]
  case class Attempt[A](fa: Parser[A]) extends Parser[Either[EpislonError, A]]

  val unit: Parser[Unit] = Pure(())

  implicit val monad: MonadError[Parser, EpislonError] =
    new StackSafeMonad[Parser] with MonadError[Parser, EpislonError] {
      def flatMap[A, B](fa: Parser[A])(f: A => Parser[B]): Parser[B] = FlatMap(fa, f)

      def pure[A](x: A): Parser[A] = Pure(x)

      def raiseError[A](e: EpislonError): Parser[A] = Raise(e)

      def handleErrorWith[A](
        fa: Parser[A]
      )(
        f: EpislonError => Parser[A]
      ): Parser[A] = attempt(fa).flatMap(_.fold(f, pure))

      override def attempt[A](fa: Parser[A]): Parser[Either[EpislonError, A]] = Attempt(fa)

    }

  def const(bits: List[Bit]): Parser[Unit] =
    (nBits(bits.size), Parser.index).mapN {
      case (actual, _) if actual == bits => Right(())
      case (actual, i)                   => Left(EpislonError(s"Expected $bits, got $actual", i))
    }.rethrow

  def nBits(n: Int): Parser[List[Bit]] = bit.replicateA(n)
  val bit: Parser[Bit] = Bit
  val index: Parser[Int] = Index

  def raiseMessage(msg: String): Parser[Nothing] = index.flatMap(i => Raise(EpislonError(msg, i)))
}

def parseBits(bits: List[Bit]): Int = Integer.parseInt(bits.map(_.toChar).mkString, 2)

case class Version(value: Int)

sealed trait Packet extends Product with Serializable {

  def fold[A](literal: Version => A, operator: (Version, List[A]) => A): A = {
    def recurse(packet: Packet) = packet.fold(literal, operator)

    this match {
      case Literal(v, _)              => literal(v)
      case Operator(version, _, subs) => operator(version, subs.map(recurse))
    }
  }

}

case class Operator(version: Version, tpe: List[Bit], subs: List[Packet]) extends Packet

case class Literal(version: Version, value: List[Bit]) extends Packet {
  def valueDec: Int = parseBits(value)
}

object parsers {
  import Parser._
  val version = nBits(3).map(parseBits(_)).map(Version(_))

  val literal: Parser[Literal] = {
    val content = nBits(5).takeThrough(_.head.isOne).map(_.map(_.tail).flatten)
    (
      version,
      const(List(1, 0, 0)) *>
        content,
    ).mapN(Literal.apply)
  }

  val operator: Parser[Packet] =
    (
      version,
      nBits(3),
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
  literal = _.value.toLong,
  operator = (v, children) => v.value.toLong |+| children.combineAll,
)

import util.chaining._
parsers
  .operator
  .parseUnsafe(data)
  .pipe(sumVersions)
