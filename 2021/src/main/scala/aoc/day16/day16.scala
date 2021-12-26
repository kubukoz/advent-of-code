package aoc.day16

import aoc.lib._
import cats.MonadError
import cats.data.StateT
import cats.implicits._

import java.lang

import Packet._
import Bit._

case class EpsilonError(msg: String, index: Int) extends Throwable(msg + " at index " + index)

case class ParserState[A](bits: Vector[A], index: Int) {

  def proceed: Either[EpsilonError, (A, ParserState[A])] =
    if (index < bits.length)
      Right((bits(index), copy(index = index + 1)))
    else
      Left(EpsilonError("No more bits", index))

}

object Day16 extends App {

  def parsePacket[F[_]: ParserApi: MonadError[*[_], EpsilonError]]: F[Packet] = {
    val api = ParserApi[F]
    import ParserApi.ops._

    def parseBits(bits: List[Bit]): Int = lang.Integer.parseInt(bits.map(_.toChar).mkString, 2)
    def parseBitsToLong(bits: List[Bit]): Long = lang.Long.parseLong(bits.map(_.toChar).mkString, 2)

    val version: F[Version] = api.nBits(3).map(parseBits(_)).map(Version(_))

    val literal: F[Literal] = {
      val content = api.nBits(5).takeThrough(_.head.isOne).map(_.map(_.tail).flatten)
      (
        version,
        api.const(List(1, 0, 0)) *>
          content.map(parseBitsToLong),
      ).mapN(Literal.apply)
    }

    val operator: F[Operator] = {
      def children(lengthBit: Bit): F[List[Packet]] =
        lengthBit match {
          case `_0` =>
            for {
              subpacketsLength <- api.nBits(15).map(parseBits)
              indexBefore <- api.index
              targetIndex = indexBefore + subpacketsLength
              nested <- parsePacket[F].whileM[List](api.index.map(_ < targetIndex))
            } yield nested

          case `_1` => api.nBits(11).map(parseBits).flatMap(parsePacket[F].replicateA(_))
        }

      (
        version,
        api.nBits(3).map(parseBits).map(OpType.values),
        api.bit.flatMap(children),
      ).mapN(Operator.apply)
    }

    literal
      .widen[Packet]
      .orElse(operator.widen[Packet])
  }

  def parse(input: String): Packet =
    parsePacket[StateT[Either[EpsilonError, *], ParserState[Bit], *]]
      .runA(
        ParserState(hexToBin(input), 0)
      )
      .toTry
      .get

  val hexToBin: String => Vector[Bit] = {
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

    _.flatMap(key(_)).toVector
  }

  def sumVersions(packet: Packet) = packet.fold[Long](
    literal = (version, _) => version.value,
    operator = (version, _, children) => version.value |+| children.combineAll,
  )

  def eval(packet: Packet) = packet.fold[Long](
    literal = (_, value) => value,
    operator = (_, op, children) => children.reduceLeft(op.eval),
  )

  locally {
    val parsed = parse(readAll("day16.txt"))

    assertEquals(parsed.sumVersions, 947, "Part 1")
    assertEquals(parsed.eval, 660797830937L, "Part 2")
  }
}
