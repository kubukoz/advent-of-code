package aoc.day16

import Bit._

sealed trait Bit extends Product with Serializable {
  def isOne = this == _1

  def toChar =
    this match {
      case `_1` => '1'
      case `_0` => '0'
    }

}

object Bit {
  case object _0 extends Bit
  case object _1 extends Bit

  implicit def fromOne(one: 1): Bit = _1
  implicit def fromZero(zero: 0): Bit = _0
}
