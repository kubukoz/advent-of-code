package com.kubukoz.aoc.day14

import cats.Show
import cats.data.State
import cats.effect.IO
import cats.effect.IOApp
import cats.implicits._
import com.kubukoz.aoc.Util
import fs2.Pipe
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

  @newtype case class Mask(overrides: Map[Int /* index */, Boolean]) {

    override def toString: String =
      indices.map(i => overrides.get(i).map(if (_) '1' else '0').getOrElse('X')).mkString

    def indices: Range = 0 until Mask.Bits

    def maskValue(value: Value): Value = {
      val newBits = overrides.foldLeft(Mask.longToBinary(value.value)) { case (bits, (index, sign)) =>
        bits.updated(index, sign)
      }

      Value(Mask.longFromBinary(newBits))
    }

    def maskAddress(address: Address): LazyList[Address] = {

      val swappedBits =
        overrides.filter(_._2).keySet.foldLeft(Mask.longToBinary(address.value)) { case (bits, index) =>
          bits.updated(index, true)
        }

      val floatingBits = (indices.toSet -- overrides.keySet).toList

      val functions =
        combinationsAll(floatingBits.flatMap(floatingBitToFunctions))
          .view
          .map(_.foldLeft(identity[Vector[Boolean]] _)(_ andThen _))

      functions.map(_(swappedBits)).map(Mask.longFromBinary).map(Address(_)).to(LazyList)
    }

    private def floatingBitToFunctions(bitIndex: Int): List[cats.Endo[Vector[Boolean]]] =
      List(true, false).map(newValue => _.updated(bitIndex, newValue))

    private def combinationsAll[A](seq: Seq[A]): Seq[Seq[A]] = (1 to seq.size).flatMap(seq.combinations)
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

    private val Bits = 36

    private def longToBinary(long: Long): Vector[Boolean] = {
      val bits: Vector[Boolean] = long.toBinaryString.map(_ == '1').toVector
      val padding = Mask.Bits - bits.size

      Vector.fill(padding)(false /* 0 */ ) ++ bits
    }

    private def longFromBinary(bits: Seq[Boolean]): Long =
      fs2
        .Stream
        .emits(bits.reverse)
        .map(if (_) 1L else 0L)
        .zipWithIndex
        .map { case (bit, indexFromRight) => bit << indexFromRight }
        .compile
        .foldMonoid

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

  val perform2: Instruction => State[Memory, Unit] = {
    case Instruction.SetMask(mask)            => Memory.setMask(mask)
    case Instruction.SetValue(address, value) =>
      Memory
        .getMask
        .map(_.maskAddress(address))
        .flatMap(_.traverse_(Memory.register(_, value)))
  }

  def showProgress[F[_], A]: Pipe[F, A, Unit] = _.scanMap(_ => 1).debug(e => s"Processed $e elements...").void

  def run: IO[Unit] =
    IO.ref(Memory.init)
      .flatTap { ref =>
        Util
          .streamFile[IO]("./files/day14.txt")
          .map(Instruction.parse)
          .map(perform2)
          .evalMap(ref.modifyState)
          .through(showProgress)
          .compile
          .drain
      }
      .flatMap(_.get)
      .map(_.registry.values.map(_.value: BigInt).toList.combineAll)
      .flatMap(IO.println(_))

}
