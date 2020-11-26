package com.kubukoz.aoc.day0

import cats.effect.IOApp
import cats.effect.IO
import com.kubukoz.aoc.Util

object Day0 extends IOApp.Simple {

  val run: IO[Unit] =
    Util.readFile[IO]("files/day0.txt").flatMap { file =>
      IO.println(file)
    }

}
