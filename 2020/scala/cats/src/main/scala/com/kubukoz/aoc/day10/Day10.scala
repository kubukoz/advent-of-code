package com.kubukoz.aoc.day10

import cats.data.NonEmptyList
import cats.data.State
import cats.effect.IO
import cats.effect.IOApp
import cats.implicits._
import cats.kernel.Order
import cats.kernel.Semigroup
import com.kubukoz.aoc.Util
import io.estatico.newtype.macros.newtype

object Day10 extends IOApp.Simple {

  @newtype case class Joltage(value: Int) {
    def isInputOf(out: Joltage): Boolean = ((value + 1) to (value + 3)).contains(out.value)
  }

  object Joltage {
    implicit val order: Order[Joltage] = deriving
    implicit val semigroup: Semigroup[Joltage] = deriving

    val init = Joltage(0)
  }

  val input =
    List(
      Util.readFile[IO]("./files/day10.txt").nested.map(_.toInt).value,
      IO.pure(List(16, 10, 15, 5, 1, 11, 7, 19, 6, 12, 4)),
      IO.pure(
        List(28, 33, 18, 42, 31, 14, 46, 20, 48, 47, 24, 23, 49, 45, 19, 38, 39, 11, 1, 32, 25, 35, 8, 17, 7, 9, 4,
          2, 34, 10, 3)
      )
    ).apply(0)
      .map { nums =>
        val baseAdapters = nums.map(Joltage(_))

        Joltage.init :: NonEmptyList((baseAdapters.max |+| Joltage(3)), baseAdapters)
      }

  val makePath: NonEmptyList[Joltage] => Option[NonEmptyList[Joltage]] =
    _.sorted.some.filter { path =>
      path.toList.zip(path.toList.tail).forall { case (a, b) => a.isInputOf(b) }
    }

  def countPaths[T: Order](
    basePath: NonEmptyList[T]
  )(
    canConnect: (T /* input */, T /* output */ ) => Boolean
  ): Long = {
    type S[A] = State[Map[T, Long], A]

    def get(at: T): S[Long] = State.inspect(_.apply(at))
    def put(at: T): Long => S[Unit] = value => State.modify(_ + (at -> value))

    val graph = basePath.reverse

    def calcPaths(joltage: T): S[Unit] = {
      val validOutputs = graph
        .toList
        .takeWhile(_ > joltage)
        .dropWhile(!canConnect(joltage, _))

      validOutputs.foldMapM(get) >>= put(joltage)
    }

    // Thank god for SO: https://stackoverflow.com/a/5164820
    val prog: S[Long] =
      put(graph.head)(1L) *>
        graph.tail.traverse(calcPaths) *>
        get(graph.last)

    prog.runA(Map.empty).value
  }

  def mkDifferences(joltages: List[Joltage]): Map[Int, Int] =
    joltages.zip(joltages.tail).map { case (a, b) => (a.value - b.value).abs }.groupByNel(identity).fmap(_.size)

  def part1(path: NonEmptyList[Joltage]): Long = {
    val differences = mkDifferences(path.toList)
    differences(1).toLong * differences(3).toLong
  }

  def part2(path: NonEmptyList[Joltage]): Long = countPaths(path)(_.isInputOf(_))

  def run: IO[Unit] = input
    .flatMap(makePath(_).liftTo[IO](new Throwable("Invalid path")))
    .flatMap { fullPath =>
      IO.println("Part 1: " + part1(fullPath)) *>
        IO.println("Part 2: " + part2(fullPath))
    }

}
