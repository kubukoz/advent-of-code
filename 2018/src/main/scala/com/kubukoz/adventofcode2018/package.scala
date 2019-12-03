package com.kubukoz
import java.util.concurrent.{Executors, TimeUnit}

import cats.{FlatMap, ~>}
import cats.effect._
import cats.syntax.flatMap._
import cats.syntax.functor._
import fs2.text

import scala.concurrent.{ExecutionContext, ExecutionContextExecutorService}

package object adventofcode2018 {
  def blockingEc[F[_]: Sync]: Resource[F, ExecutionContextExecutorService] =
    Resource.make(
      Sync[F].delay(
        ExecutionContext.fromExecutorService(Executors.newCachedThreadPool())
      )
    )(ec => Sync[F].delay(ec.shutdown()))

  def fileContent[F[_]: Sync: ContextShift](
    fileName: String
  ): fs2.Stream[F, String] =
    fs2.Stream.resource(blockingEc).flatMap[F, String] { ec =>
      import fs2.text

      fs2.io
        .readInputStream(
          Sync[F].delay(getClass.getResourceAsStream(fileName)),
          4192,
          ec
        )
        .through(text.utf8Decode)
    }

  def fileLines[F[_]: Sync: ContextShift](
    fileName: String
  ): fs2.Stream[F, String] =
    fileContent(fileName).through(text.lines[F])

  def modifyBetween[A](from: Int,
                       length: Int)(modify: A => A)(as: List[A]): List[A] = {
    val (before, right) = as.splitAt(from)
    val (toChange, after) = right.splitAt(length)

    before ::: toChange.map(modify) ::: after
  }

  def measure[F[_]: Clock: FlatMap: ConsoleOut, A, E](
    tag: String
  )(fa: F[A]): F[A] = {
    val now = Clock[F].monotonic(TimeUnit.MILLISECONDS)

    for {
      before <- now
      result <- fa
      after <- now
      _ <- ConsoleOut[F].putStrLn(s"$tag took ${after - before}ms")
    } yield result
  }

  def measureK[F[_]: Clock: FlatMap: ConsoleOut, E](tag: String): F ~> F =
    λ[F ~> F](measure(tag)(_))

}
