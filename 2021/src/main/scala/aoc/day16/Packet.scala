package aoc.day16

import Packet._
import cats.implicits._

sealed trait Packet extends Product with Serializable {

  def fold[A](
    literal: (Packet.Version, Long) => A,
    operator: (Packet.Version, OpType, List[A]) => A,
  ): A = {
    def recurse(packet: Packet) = packet.fold(literal, operator)

    this match {
      case Literal(version, value)     => literal(version, value)
      case Operator(version, op, subs) => operator(version, op, subs.map(recurse))
    }
  }

  def sumVersions = fold[Long](
    literal = (version, _) => version.value,
    operator = (version, _, children) => version.value |+| children.combineAll,
  )

  def eval = fold[Long](
    literal = (_, value) => value,
    operator = (_, op, children) => children.reduceLeft(op.eval),
  )

}

object Packet {
  final case class Operator(version: Version, tpe: OpType, subs: List[Packet]) extends Packet
  final case class Literal(version: Version, value: Long) extends Packet

  final case class Version(value: Long)

}
