package com.kubukoz.aoc.day15

import cats.Show
import cats.data.NonEmptyList
import cats.data.NonEmptySet
import cats.data.State
import cats.effect.IO
import cats.effect.IOApp
import cats.implicits._
import cats.kernel.CommutativeSemigroup
import cats.kernel.Order
import cats.kernel.Semigroup
import io.estatico.newtype.macros.newtype
import monocle.macros.Lenses

object Day15 extends IOApp.Simple {
  @newtype case class SeenNumber(value: Int)

  object SeenNumber {
    implicit val order: Order[SeenNumber] = deriving

    def fromLastSeen(lastSeen: Option[SeenIndex], currentIndex: SeenIndex): SeenNumber =
      SeenNumber(lastSeen.foldMap(currentIndex.value - _.value))
  }

  //a set of up to 2 elements (effectively 1 or 2), leaving the largest one in the semigroup.
  @newtype case class Max2Set[A] private (values: NonEmptySet[A])

  object Max2Set {

    def one[A: Order](a: A): Max2Set[A] = Max2Set(NonEmptySet.one(a))

    implicit def semigroup[A: Order]: CommutativeSemigroup[Max2Set[A]] = (a, b) => {
      Max2Set(
        NonEmptySet.fromSetUnsafe(
          (a.values ++ b.values).toList.sorted.takeRight(2).to(collection.immutable.SortedSet)
        )
      )
    }

  }

  @newtype case class SeenIndex(value: Int) {
    def next: SeenIndex = SeenIndex(value + 1)
  }

  object SeenIndex {
    implicit val order: Order[SeenIndex] = deriving
  }

  @Lenses
  case class Info(
    appearances: Map[SeenNumber, Max2Set[SeenIndex]],
    currentIndex: Int,
    currentNumber: SeenNumber
  )

  object Info {

    implicit val show: Show[Info] = Show.fromToString

    def fromPrefix(prefix: NonEmptyList[Int]): Info = Info(
      appearances = prefix.map(SeenNumber(_)).zipWithIndex.toList.toMap.fmap(i => Max2Set.one(SeenIndex(i))),
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
        modAppearances(_ |+| Map(number -> Max2Set.one(atIndex)))
      }

    def getAppearances(number: SeenNumber): S[Max2Set[SeenIndex]] =
      State.inspect(_.appearances(number))

    val getNumber: S[SeenNumber] = State.inspect(Info.currentNumber.get)
    val getIndex: S[SeenIndex] = State.inspect(Info.currentIndex.get).map(SeenIndex(_))

    def modAppearances(
      f: Map[SeenNumber, Max2Set[SeenIndex]] => Map[SeenNumber, Max2Set[SeenIndex]]
    ): S[Unit] = State.modify(Info.appearances.modify(f))

  }

  def turn: S[Unit] =
    for {
      currentIndex   <- S.getIndex
      allAppearances <- S.getNumber.flatMap(S.getAppearances)

      lastAppearance = allAppearances.values.toSortedSet.excl(currentIndex).maxOption
      nextNumber = SeenNumber.fromLastSeen(lastAppearance, currentIndex)

      _ <- S.bumpIndex
      _ <- S.appear(nextNumber)
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
    run(30000000 / 30)
    // runAll(10)
  }

}
