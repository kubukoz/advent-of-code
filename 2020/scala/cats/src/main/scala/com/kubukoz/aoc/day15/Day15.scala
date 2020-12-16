package com.kubukoz.aoc.day15

import cats.Show
import cats.data.NonEmptyList
import cats.data.State
import cats.effect.IO
import cats.effect.IOApp
import cats.implicits._
import cats.kernel.Order
import cats.kernel.Semigroup
import io.chrisdavenport.semigroups.Max
import io.estatico.newtype.macros.newtype
import monocle.macros.Lenses

object Day15 extends IOApp.Simple {
  @newtype case class SeenNumber(value: Int)

  object SeenNumber {
    def fromLastSeen(lastSeen: Option[SeenIndex], currentIndex: SeenIndex): SeenNumber =
      SeenNumber(lastSeen.foldMap(currentIndex.value - _.value))
  }

  @newtype case class SeenIndex(value: Int) {
    def next: SeenIndex = SeenIndex(value + 1)
  }

  object SeenIndex {
    implicit val order: Order[SeenIndex] = deriving
  }

  @Lenses
  case class Info(
    appearances: Map[SeenNumber, Max[SeenIndex]],
    currentIndex: Int,
    currentNumber: SeenNumber
  )

  object Info {

    implicit val show: Show[Info] = Show.fromToString

    def fromPrefix(prefix: NonEmptyList[Int]): Info = Info(
      appearances = prefix.map(SeenNumber(_)).zipWithIndex.toList.toMap.fmap(i => Max(SeenIndex(i))),
      currentIndex = prefix.toList.indices.last,
      currentNumber = SeenNumber(prefix.last)
    )

  }

  type S[A] = State[Info, A]

  object S {
    val bumpIndex: S[Unit] = State.modify(Info.currentIndex.modify(_ + 1))
    def setNumber(number: SeenNumber): S[Unit] = State.modify(Info.currentNumber.set(number))

    def appear(number: SeenNumber): S[Unit] =
      getIndex.flatMap { atIndex =>
        modAppearances(_ |+| Map(number -> Max(atIndex)))
      }

    def getAppearances(number: SeenNumber): S[Option[SeenIndex]] =
      State.inspect(_.appearances.get(number).map(_.getMax))

    val getNumber: S[SeenNumber] = State.inspect(Info.currentNumber.get)
    val getIndex: S[SeenIndex] = State.inspect(Info.currentIndex.get).map(SeenIndex(_))

    def modAppearances(
      f: Map[SeenNumber, Max[SeenIndex]] => Map[SeenNumber, Max[SeenIndex]]
    ): S[Unit] = State.modify(Info.appearances.modify(f))

  }

  def turn: S[Unit] =
    for {
      currentIndex   <- S.getIndex
      prevNumber     <- S.getNumber
      lastAppearance <- S.getAppearances(prevNumber)

      nextNumber = SeenNumber.fromLastSeen(lastAppearance, currentIndex)

      _ <- S.appear(prevNumber)
      _ <- S.bumpIndex
      _ <- S.setNumber(nextNumber)
    } yield ()

  def run: IO[Unit] = IO.println {
    // val input = Info.fromPrefix("1,20,11,6,12,0".split(",").map(_.toInt).toList.toNel.get)
    val prefix = "0,3,6".split(",").map(_.toInt).toList.toNel.get
    // val prefix = "1,20,11,6,12,0".split(",").map(_.toInt).toList.toNel.get

    val input = Info.fromPrefix(prefix)

    implicit def stateSemigroup[A]: Semigroup[State[A, Unit]] = _ >> _

    def runAll(totalRounds: Int) =
      prefix.toList ++ ((turn *> S.getNumber).replicateA(totalRounds - prefix.size)).runA(input).value.map(_.value)

    def run(totalRounds: Int) =
      Semigroup.combineN(turn, totalRounds - prefix.size).runS(input).value.currentNumber.value

    // (run(2020), run(30000000))
    run(30000000)
    // runAll(10)
  }

}
