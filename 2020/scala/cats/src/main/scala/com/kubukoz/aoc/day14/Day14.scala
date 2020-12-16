package com.kubukoz.aoc.day14

import cats.Show
import cats.data.State
import cats.effect.IO
import cats.effect.IOApp
import cats.implicits._
import cats.kernel.Monoid
import com.kubukoz.aoc.Util
import io.estatico.newtype.macros.newtype
import monocle.macros.Lenses

object Day14 extends IOApp.Simple {

  def debug(mappings: (String, Any)*) = {
    val msg = mappings.map { case (k, v) => s"$k: $v" }.mkString("\n")

    println("\n" + msg + "\n\n")
  }

  sealed trait Instruction

  @newtype case class Address(value: Long)

  @newtype case class Value(value: Long)

  object Value {
    implicit val show: Show[Value] = deriving
    implicit val monoid: Monoid[Value] = deriving
  }

  @newtype case class Mask(overrides: Map[Int /* index */, Boolean]) {

    //for debugging purposes
    def asString: String =
      (0 until 36).map(i => overrides.get(i).map(if (_) '1' else '0').getOrElse('X')).mkString

    def maskValue(value: Value): Value = {
      val bits: Vector[Boolean] = value.value.toBinaryString.map(_ == '1').toVector
      val padding = 36 - bits.size

      val padded = List.fill(padding)(false) ++ bits

      val newBits = overrides.foldLeft(padded) { case (bits, (index, sign)) =>
        bits.updated(index, sign)
      }

      Value(longFromBinary(newBits))
    }

    def maskAddress(address: Address): List[Address] = ???

    private def longFromBinary(bits: List[Boolean]): Long =
      fs2
        .Stream
        .emits(bits.reverse)
        .map(if (_) 1L else 0L)
        .zipWithIndex
        .map { case (bit, indexFromRight) => bit << indexFromRight }
        .compile
        .foldMonoid

  }

  object Mask {

    val parse: String => Mask = {
      val mapping = Map('0' -> false, '1' -> true)

      str =>
        Mask(
          str
            .map(mapping.get)
            .zipWithIndex
            .map(_.swap)
            .collect { case (k, Some(v)) => (k, v) }
            .toMap
        )
    }

    // it'll be overwritten anyway on the first line, so the contents can be arbitrary
    val init = Mask(Map.empty)
  }

  object Instruction {
    final case class SetMask(mask: Mask) extends Instruction
    final case class SetValue(address: Address, value: Value) extends Instruction

    val parse: String => Instruction = {
      case s"mask = $maskText"       => SetMask(Mask.parse(maskText))
      case s"mem[$address] = $value" => SetValue(Address(address.toLong), Value(value.toLong))
    }

  }

  @Lenses
  final case class Memory(mask: Mask, registry: Map[Address, Value])

  object Memory {

    val init = Memory(
      Mask.init,
      Map.empty
    )

    def setMask(mask: Mask): State[Memory, Unit] = State.modify(Memory.mask.set(mask))
    val getMask: State[Memory, Mask] = State.inspect(Memory.mask.get)
    def register(address: Address, finalValue: Value): State[Memory, Unit] =
      State.modify(Memory.registry.modify(_ + (address -> finalValue)))

  }

  val perform: Instruction => State[Memory, Unit] = {
    case Instruction.SetMask(mask)            => Memory.setMask(mask)
    case Instruction.SetValue(address, value) =>
      Memory
        .getMask
        .map(_.maskValue(value))
        .flatMap(Memory.register(address, _))
  }

  def run: IO[Unit] =
    IO.ref(Memory.init)
      .flatTap { ref =>
        Util
          .streamFile[IO]("./files/day14.txt")
          .map(Instruction.parse)
          .map(perform)
          .evalMap(ref.modifyState)
          .compile
          .drain
      }
      .flatMap(_.get)
      .map(_.registry.values.map(_.value: BigInt).toList.combineAll)
      .flatMap(IO.println(_))

}
