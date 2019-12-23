package com.kubukoz.aoc

import cats.effect.Sync
import cats.effect.ContextShift
import cats.effect.Blocker
import java.nio.file.Paths

object Util {

  def readFile[F[_]: Sync: ContextShift](name: String): F[String] =
    fs2.Stream
      .resource(Blocker[F])
      .flatMap(fs2.io.file.readAll(Paths.get(name), _, 4096))
      .through(fs2.text.utf8Decode[F])
      .compile
      .string
}
