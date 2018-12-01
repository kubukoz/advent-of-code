package com.kubukoz
import java.util.concurrent.Executors

import cats.effect.{ContextShift, IO, Resource, Sync}

import scala.concurrent.{ExecutionContext, ExecutionContextExecutorService}

package object adventofcode2017 {
  def blockingEc[F[_]: Sync]: Resource[F, ExecutionContextExecutorService] =
    Resource.make(
      Sync[F].delay(
        ExecutionContext.fromExecutorService(Executors.newCachedThreadPool())
      )
    )(ec => Sync[F].delay(ec.shutdown()))

  def fileLines[F[_]: Sync: ContextShift](
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
        .through(text.lines[F])
    }
}
