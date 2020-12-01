package com.kubukoz.aoc

import cats.effect.Concurrent
import java.nio.file.Paths
import fs2.io.file.Files
import cats.effect.kernel.Ref
import cats.Monad
import cats.mtl.Stateful
import cats.syntax.all._

object Util {

  def readFile[F[_]: Files: Concurrent](name: String): F[List[String]] =
    Files[F]
      .readAll(Paths.get(name), 4096)
      .through(fs2.text.utf8Decode[F])
      .through(fs2.text.lines[F])
      .compile
      .toList

  def state[F[_]: Ref.Make: Monad, A](start: A): F[Stateful[F, A]] =
    Ref[F].of(start).map { ref =>
      new Stateful[F, A] {
        def monad: Monad[F] = implicitly
        def get: F[A] = ref.get
        def set(s: A): F[Unit] = ref.set(s)
        override def modify(f: A => A): F[Unit] = ref.update(f)
      }
    }

}
