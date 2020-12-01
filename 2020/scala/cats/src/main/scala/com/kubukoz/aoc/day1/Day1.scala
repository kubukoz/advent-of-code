package com.kubukoz.aoc.day1

import cats.effect.IOApp
import cats.effect.IO
import com.kubukoz.aoc.Util
import cats.implicits._

object Day1 extends IOApp.Simple {

  def run: IO[Unit] =
    Util
      .readFile[IO]("files/day1.txt")
      .map(_.map(_.trim).filter(_.nonEmpty).map(_.toInt))
      .map { nums =>
        List(2, 3)
          .map {
            nums
              .combinations(_)
              .find(_.sum == 2020)
              .map(_.product)
          }
      }
      .flatMap(_.traverse_(IO.println(_)))

}
